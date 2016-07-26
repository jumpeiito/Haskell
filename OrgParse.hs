{-# LANGUAGE FlexibleContexts #-}

module OrgParse (dateFold, parseToDayList) where 

import Strdt                            (strdt, toYear, toMonth, toDay, todayDay)
import Data.Time                        (Day (..), fromGregorian)
import Data.Maybe                       (isJust, mapMaybe)
import Text.Printf                      (printf)
import Control.Monad.Writer
import Control.Applicative              hiding (many, (<|>))
import Text.Parsec
import Text.Parsec.String
import qualified Data.ByteString.Char8  as B
import qualified Data.Map               as Map
import qualified Util                   as U
import qualified System.IO              as I
import qualified Control.Monad.State    as St
import qualified Text.StringLike        as Like

orgDir :: FilePath
orgDir = "f:/Org/news/"

orgFileName :: Integer -> Int -> FilePath
orgFileName = printf "%s%04d%02d.org" orgDir 
----------------------------------------------------------------------------------------------------
data Lines s =
  OrgDate s
  | OrgTitle s
  | OrgLine s
  | OrgArticle { time   :: Maybe Day,
                 header :: s,
                 body   :: [s],
                 paper' :: s }
  | OrgError deriving (Show, Eq)

instance (Like.StringLike s, Monoid s) => Monoid (Lines s) where
  mempty = makeOrgArticle
  OrgDate s          `mappend` art@(OrgArticle{}) = art { time = strdt s :: Maybe Day }
  OrgTitle s         `mappend` art@(OrgArticle{}) = takeHeader art s
  OrgLine s          `mappend` art@(OrgArticle{}) = art { body = body art ++ [s] }
  art@(OrgArticle{}) `mappend` OrgArticle{} = art
  _ `mappend` _ = mempty

makeOrgArticle :: (Like.StringLike a, Monoid a) => Lines a
makeOrgArticle = OrgArticle { time   = Nothing,
                              header = mempty,
                              body   = mempty,
                              paper' = mempty }
----------------------------------------------------------------------------------------------------
leapYear :: Integer -> Bool
leapYear y
  | y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y `mod` 4 == 0 = True
  | otherwise = False

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

titleP = do
  string "** "
  t <- aChar
  return $ OrgTitle t

dateP :: Parser (Lines String)
dateP = do
  string "* "
  d <- manyTill (oneOf "0123456789/-") eof
  return $ OrgDate d

lineP :: Parser (Lines String)
lineP = do
  s <- manyTill anyChar eof
  return $ OrgLine s

toLine :: Like.StringLike a => a -> Lines String
toLine s = case parse selected "" target of
  Right line -> line
  Left _     -> OrgError
  where selected = choice [try titleP, try dateP, lineP]
        target   = Like.castString s
----------------------------------------------------------------------------------------------------
type PaperMap a = Map.Map Int [a]

papers :: Like.StringLike a => [a]
papers = map Like.castString $ 
         (++"新聞") <$> ["朝日", "毎日", "読売", "日経", "産経"]

takeHeader :: (Like.StringLike a, Monoid a) => Lines a -> a -> Lines a
takeHeader art head' =
  art { header = head', paper' = p }
  where p = takePaper head'

takePaper :: (Like.StringLike a, Monoid a) => a -> a
takePaper s = undefined-- case parse takePaperParse mempty s of
  -- Right s' -> s'
  -- Left _   -> mempty

takePaperParse :: Parser String
takePaperParse = try inner <|> (anyChar >> takePaperParse)
  where inner = do
        char '['
        rel <- many1 (noneOf "]")
        char ']'
        return rel

makePaperMap :: (Like.StringLike a, Monoid a) => [Lines a] -> PaperMap a
makePaperMap = U.makeMap (timeToDay . time) paper'
  where timeToDay Nothing = 0
        timeToDay (Just d) = toDay d

paperElem :: (Like.StringLike a, Monoid a) => Int -> PaperMap a -> a -> Bool
paperElem day mp p = isJust $ elem p <$> Map.lookup day mp

notElemDayPaper :: (Like.StringLike a, Monoid a) => [Int] -> [Lines a] -> [(Int, [a])]
notElemDayPaper dayList x = execWriter $ do
  let mp = makePaperMap x
  let pl = papers
  forM_ dayList $ \day -> do
    let restPaper = filter (not . paperElem day mp) pl
    if null restPaper
      then tell []
      else tell [(day, restPaper)]
  return ()
----------------------------------------------------------------------------------------------------
dateFold :: (Like.StringLike a, Monoid a) => [Lines a] -> [Lines a]
dateFold s = thd . (`St.execState` (mempty, mempty, [])) $ do
  St.forM_ s $ \n -> do
    (prev, art, big) <- St.get
    case n of
      OrgDate s  -> St.put (n, n <> mempty, big <> [art])
      OrgTitle s -> St.put (prev, n <> (prev <> mempty), big <> [art])
      _          -> St.put (prev, n <> art, big)
  return ()
  where thd (_, _, a) = a

orgDateList :: [Lines s] -> [Int]
orgDateList = map toDay . U.uniq . mapMaybe time

notElemDay :: [Int] -> [Lines s] -> [Int]
notElemDay dayList x = [ y | y <- dayList, y `notElem` orgDateList x]
----------------------------------------------------------------------------------------------------
orgLineList :: (Like.StringLike a, Monoid a) => a -> [Lines String]
orgLineList = dateFold . map toLine . lines . Like.castString

parseToDayList :: Integer -> Int -> IO [Day]
parseToDayList year month = do
  today    <- todayDay
  let file    = orgFileName year month
  let daylist = makeMonthList today year month
  contents <- orgLineList <$> U.readUTF8File file
  return $ map (fromGregorian year month) $ notElemDay daylist contents


