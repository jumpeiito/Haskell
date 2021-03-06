{-# LANGUAGE FlexibleContexts #-}

-- module Snews.OrgParse (dateFold, parseToDayList) where 
module Snews.OrgParse  where 

import Util                             (makeMap, uniq, readUTF8File, FileDirect (..), (<^>), allfd)
import Util.Strdt                       (strdt, toYear, toMonth, toDay, todayDay)
import Util.StrEnum                     (split)
import Data.Time                        (Day (..), fromGregorian)
import Data.Maybe                       (isJust, mapMaybe)
import Data.Sequence                    ((|>))
import qualified Data.Sequence          as Seq
import Text.Printf                      (printf)
import Control.Monad.Writer
import Control.Concurrent.Async
import Text.StringLike                  (castString, StringLike (..))
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Reader
import System.Directory                 (doesFileExist)
import qualified Data.Map               as Map
import qualified Control.Monad.State    as St
-- import qualified System.IO              as I

data OrgConfig = OC { topdir    :: FilePath
                    , basename  :: String
                    , condition :: FileDirect }

config :: OrgConfig
config = OC { topdir    = "C:/Users/Jumpei/org/news/"
            , basename  = "%s%04d%02d.org"
            , condition = (FD (const True) ("org" <^>))}

orgFileName :: Integer -> Int -> Reader OrgConfig FilePath
orgFileName year month = do
  dir  <- topdir <$> ask
  base <- basename <$> ask
  return $ printf base dir year month
----------------------------------------------------------------------------------------------------
data Lines s =
  OrgDate s
  | OrgTitle s
  | OrgLine s
  | OrgArticle { time   :: Maybe Day
               , header :: s
               , body   :: Seq.Seq s
               , paper' :: s }
  | OrgError deriving (Show, Eq)

type Days = [Int]
type Contents a = [Lines a]

instance (StringLike s, Monoid s) => Monoid (Lines s) where
  mempty = makeOrgArticle
  OrgDate s        `mappend` art@OrgArticle{} = art { time = strdt s }
  OrgTitle s       `mappend` art@OrgArticle{} = takeHeader art s
  OrgLine s        `mappend` art@OrgArticle{} = art { body = body art |> s }
  art@OrgArticle{} `mappend` OrgArticle{}     = art
  _ `mappend` _ = mempty

makeOrgArticle :: (StringLike a, Monoid a) => Lines a
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
aChar :: Parser String
aChar = many1 (noneOf "\r\n")

selectedParse :: Parser (Lines String)
selectedParse = do
  try (OrgTitle <$> (string "** " *> aChar))
  <|> try (OrgDate <$> (string "* " *> manyTill (oneOf "0123456789/-") eof))
  <|> OrgLine <$> (many1 anyChar)

toLine :: StringLike a => a -> Lines String
toLine s = either (const OrgError) id 
                  $ parse selectedParse "" target
  where target   = castString s
----------------------------------------------------------------------------------------------------
type PaperMap a = Map.Map Int [a]

papers :: StringLike a => [a]
papers = map castString $ 
         (++"新聞") <$> ["朝日", "毎日", "読売", "日経", "産経"]

takeHeader :: (StringLike a, Monoid a) => Lines a -> a -> Lines a
takeHeader art head' = art { header = head', paper' = p }
  where p = takePaper head'

takePaper :: (StringLike a, Monoid a) => a -> a
takePaper s = case parse takePaperParse mempty (castString s) of
  Right s' -> castString s'
  Left _   -> mempty

takePaperParse :: Parser String
takePaperParse = try inner <|> (anyChar >> takePaperParse)
  where inner = between (char '[')
                        (char ']')
                        (many1 (noneOf "]"))

makePaperMap :: (StringLike a, Monoid a) => Contents a -> PaperMap a
makePaperMap = makeMap (timeToDay . time) paper'
  where timeToDay Nothing = 0
        timeToDay (Just d) = toDay d

paperElem :: (StringLike a, Monoid a) => Int -> PaperMap a -> a -> Bool
paperElem day mp p = isJust $ elem p <$> Map.lookup day mp

notElemDayPaper :: (StringLike a, Monoid a) => Days -> Contents a -> [(Int, [a])]
notElemDayPaper dayList x = execWriter $ do
  let mp = makePaperMap x
  let pl = papers
  forM_ dayList $ \day -> do
    let restPaper = filter (not . paperElem day mp) pl
    if null restPaper
      then tell mempty
      else tell [(day, restPaper)]
----------------------------------------------------------------------------------------------------
dateFold :: (StringLike a, Monoid a) => Contents a -> Contents a
dateFold s = thd . (`St.execState` (mempty, mempty, [])) $ 
  St.forM_ s $ \n -> do
    (prev, art, big) <- St.get
    case n of
      OrgDate _  -> St.put (n, n <> mempty, big <> [art])
      OrgTitle _ -> St.put (prev, n <> (prev <> mempty), big <> [art])
      _          -> St.put (prev, n <> art, big)
  where thd (_, _, a) = a

orgDateList :: Contents a -> Days
orgDateList = map toDay . uniq . mapMaybe time

notElemDay :: Days -> Contents a -> Days
notElemDay dayList x = [ y | y <- dayList, y `notElem` orgDateList x]
----------------------------------------------------------------------------------------------------
orgLineList :: (StringLike a, Monoid a) => a -> Contents String
orgLineList = dateFold . map toLine . lines . castString

parseToDayList :: Integer -> Int -> IO [Day]
parseToDayList year month = do
  today    <- todayDay
  let orgFile = orgFileName year month `runReader` config
  orgFileExist <- doesFileExist orgFile
  case orgFileExist of
    False -> error $ printf "orgfile(%s) doesn't exists! from OrgParse.hs" orgFile
    True -> do
      contents <- orgLineList <$> readUTF8File orgFile
      let daylist = makeMonthList today year month
      let days    = notElemDay daylist contents
      return $ map (fromGregorian year month) days
----------------------------------------------------------------------------------------------------
headerTags :: String -> [String]
headerTags head = case split ':' head of
  [_] -> []
  x   -> init $ tail x

fileToTags :: FilePath -> IO [String]
fileToTags fp = do
  let extract = uniq . concatMap (headerTags . header)
  extract <$> orgLineList <$> readUTF8File fp

collectTags :: IO [String]
collectTags = (`runReaderT` config) $ do
  dir   <- topdir <$> ask
  cond  <- condition <$> ask
  files <- liftIO $ allfd dir cond
  thunk <- liftIO $ mapM (async . fileToTags) files
  liftIO $ uniq . concat <$> mapM wait thunk

tagsOutput1 :: IO String
tagsOutput1 = do
  tags <- collectTags
  return $
    "(setq org-newspaper-tags-list '("     ++
    (concat $ map (printf "\"%s\" ") tags) ++
    "))"
