{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Snews.OrgConduit where

import           Util 
import           Util.Strdt
import           Data.Time
import           Data.Monoid
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Applicative
import           Text.Printf
import           Data.Attoparsec.ByteString hiding (satisfy)
import           Data.Attoparsec.ByteString.Char8
import           Data.Conduit
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.ByteString.Char8  as B
import qualified Data.Conduit.List      as CL
import qualified Data.Conduit.Binary    as CB
import           Data.Sequence          ((|>))
import qualified Data.Sequence          as Seq
import           System.Directory (doesFileExist)

data OrgConfig = OC { topdir    :: FileSystem
                    , basename  :: String
                    , condition :: FileDirect }

config = OC { topdir = Directory [ "C:/Users/Jumpei/Org/news/"
                                 , "d:/home/Org/news/"]
            , basename = "%s%04d%02d.org"
            , condition = (FD (const True) ("org" <^>)) }

orgDir :: ReaderT OrgConfig IO (Maybe FilePath)
orgDir = do
  directory <- topdir <$> ask
  liftIO $ runFile directory

orgFile :: Integer -> Int -> ReaderT OrgConfig IO FilePath
orgFile year month = do
  dir <- orgDir
  case dir of
    Nothing -> error "orgdirectory not found."
    Just orgDirectory -> do
      base <- basename <$> ask
      return $ printf base orgDirectory year month
----------------------------------------------------------------------------------------------------
data Lines =
  OrgDate B.ByteString
  | OrgTitle B.ByteString
  | OrgLine B.ByteString
  | OrgArticle { time   :: Maybe Day
               , header :: B.ByteString
               , body   :: Seq.Seq B.ByteString
               , paper' :: B.ByteString }
  | OrgError deriving (Show, Eq)

type Days      = [Int]
type Contents  = [Lines]
type NewsPaper = B.ByteString

instance Monoid Lines where
  mempty = makeOrgArticle
  OrgDate s        `mappend` art@OrgArticle{} = art { time = strdt s }
  OrgTitle s       `mappend` art@OrgArticle{} = takeHeader art s
  OrgLine s        `mappend` art@OrgArticle{} = art { body = body art |> s }
  art@OrgArticle{} `mappend` OrgArticle{}     = art
  _ `mappend` _ = mempty

makeOrgArticle :: Lines
makeOrgArticle = OrgArticle { time   = Nothing
                            , header = mempty
                            , body   = mempty
                            , paper' = mempty }

----------------------------------------------------------------------------------------------------
leapYear :: Integer -> Bool
leapYear y
  | y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y == 0                               = False
  | y `mod` 4 == 0                       = True
  | otherwise                            = False

monthEnd :: Integer -> Int -> Int
monthEnd year month
  | month == 1  ||
    month == 3  ||
    month == 5  ||
    month == 7  ||
    month == 8  ||
    month == 10 ||
    month == 12 = 31
  | month == 4  ||
    month == 6  ||
    month == 9  ||
    month == 11 = 30
  | month == 2 = if leapYear year then 29 else 28
  | otherwise = -1

makeMonthList :: Day -> Integer -> Int -> [Int]
makeMonthList day year month
  | year == y && month == m = [1..d]
  | otherwise = [1..(monthEnd year month)]
  where y = toYear day
        m = toMonth day
        d = toDay day
----------------------------------------------------------------------------------------------------
-- aChar :: Parser B.ByteString
-- aChar = B.pack <$> many1 (noneOf "\r\n")

-- selectedParse :: Parser Lines
-- selectedParse = do
--   try (OrgTitle <$> (string "** " *> aChar))
--   <|> try (OrgDate <$> B.pack <$> (string "* " *> many1 (oneOf "0123456789/-")))
--   <|> OrgLine <$> B.pack <$> (many1 anyChar)

-- toLine :: B.ByteString -> Lines
-- toLine s = either (const OrgError) id 
--                   $ parse selectedParse "" s
  -- where target   = castString s
----------------------------------------------------------------------------------------------------
type PaperMap = Map.Map Int [B.ByteString]

papers :: [B.ByteString]
papers = (<>"新聞") <$> ["朝日", "毎日", "読売", "日経", "産経"]

takeHeader :: Lines -> B.ByteString -> Lines
takeHeader art head' = art { header = head', paper' = p }
  where p = takePaper head'

takePaper :: B.ByteString -> B.ByteString
takePaper s = case parse takePaperParse s of
                Done _ p -> p
                _ -> mempty

between beginf endf bodyf = beginf *> bodyf <* endf

takePaperParse :: Parser B.ByteString
takePaperParse = try (B.pack <$> inner) <|> (anyChar >> takePaperParse)
  where inner = between (char '[')
                        (char ']')
                        (many1 (satisfy (/= ']')))

-- makePaperMap :: Contents -> PaperMap
-- makePaperMap = makeMap (timeToDay . time) paper'
--   where timeToDay Nothing = 0
--         timeToDay (Just d) = toDay d

-- paperElem :: Int -> PaperMap -> NewsPaper -> Bool
-- paperElem day mp p = isJust $ elem p <$> Map.lookup day mp

-- notElemDayPaper :: Days -> Contents -> [(Int, [B.ByteString])]
-- notElemDayPaper dayList x = execWriter $ do
--   let mp = makePaperMap x
--   let pl = papers
--   forM_ dayList $ \day -> do
--     let restPaper = filter (not . paperElem day mp) pl
--     if null restPaper
--       then tell mempty
--       else tell [(day, restPaper)]
----------------------------------------------------------------------------------------------------
-- dateFold :: Contents -> Contents
-- dateFold s = thd . (`St.execState` (mempty, mempty, [])) $ 
--   St.forM_ s $ \n -> do
--     (prev, art, big) <- St.get
--     case n of
--       OrgDate _  -> St.put (n, n <> mempty, big <> [art])
--       OrgTitle _ -> St.put (prev, n <> (prev <> mempty), big <> [art])
--       _          -> St.put (prev, n <> art, big)
--   where thd (_, _, a) = a

-- orgDateList :: Contents -> Days
-- orgDateList = map toDay . uniq . mapMaybe time

-- notElemDay :: Days -> Contents -> Days
-- notElemDay dayList x = [ y | y <- dayList, y `notElem` orgDateList x]

notElemDay2 :: Days -> [Maybe Day] -> Days
notElemDay2 fromMonth fromContents =
  [ y | y <- fromMonth, y `notElem` days]
  where days = map toDay $ catMaybes fromContents
----------------------------------------------------------------------------------------------------
-- orgLineList :: B.ByteString -> Contents
-- orgLineList = dateFold . map toLine . B.lines

orgSource :: MonadResource m => FilePath -> Source m B.ByteString
orgSource file = CB.sourceFile file $= CB.lines

-- orgToLine :: MonadResource m => Conduit B.ByteString m Lines
-- orgToLine = do
--   line <- await
--   case line of
--     Nothing -> return mempty
--     Just l  -> yield (toLine l) >> orgToLine

-- orgContents :: MonadResource m => Sink Lines m Contents
-- orgContents = do
--   list <- CL.consume
--   return $ dateFold list

oneOf xs = foldl1 (<|>) (map char xs)

orgDayConsumer :: Monad m => Consumer B.ByteString (ResourceT m) [Maybe Day]
orgDayConsumer = do
  CL.fold orgFunc []
  where dateParse = string "* " *> many1 (oneOf "0123456789/-") 
        orgFunc xs line = case parse dateParse line `feed` mempty of
                            Done _ d -> (strdt d) : xs
                            _ -> xs

parseToDayList :: Integer -> Int -> IO [Day]
parseToDayList year month = do
  today        <- todayDay
  orgFile      <- orgFile year month `runReaderT` config
  orgFileExist <- doesFileExist orgFile

  case orgFileExist of
    False -> error $ printf "orgfile(%s) doesn't exists! from OrgParse.hs" orgFile
    True -> do
      orgArticleDays <- runResourceT $
        orgSource orgFile
        $$ orgDayConsumer

      let daylist = makeMonthList today year month
      let days    = notElemDay2 daylist orgArticleDays
      return $ map (fromGregorian year month) days
