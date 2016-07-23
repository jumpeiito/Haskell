{-# LANGUAGE FlexibleContexts #-}

module OrgParse (dateFold, parseToDayList) where 

import Data.Time
import Data.Monoid
import Data.Maybe
import Strdt
-- import NewsArticle.Base
import qualified Data.Map               as Map
-- import qualified Data.List.Split as Sp
import qualified Util                   as U
import qualified System.IO              as I
import qualified Control.Monad.State    as St
import qualified Data.ByteString.Char8  as B
import Control.Monad.Writer
import Control.Applicative hiding (many, (<|>))
import Text.Printf
import Text.Parsec
import Text.Parsec.String

test :: String
test = "f:/Org/news/201607.org"

orgDir :: FilePath
orgDir = "f:/Org/news/"

orgFileName :: Integer -> Int -> FilePath
orgFileName = printf "%s%04d%02d.org" orgDir 
----------------------------------------------------------------------------------------------------
data Lines s =
  OrgDate String
  | OrgTitle String
  | OrgLine String
  | OrgArticle { time   :: Maybe Day,
                 header :: String,
                 body   :: [String],
                 paper' :: String }
  | OrgError deriving (Show, Eq)

instance Monoid (Lines s) where
  mempty = makeOrgArticle
  OrgDate s          `mappend` art@(OrgArticle{}) = art { time = strdt s :: Maybe Day }
  OrgTitle s         `mappend` art@(OrgArticle{}) = takeHeader art s
  OrgLine s          `mappend` art@(OrgArticle{}) = art { body = body art ++ [s] }
  art@(OrgArticle{}) `mappend` OrgArticle{} = art
  _ `mappend` _ = mempty

makeOrgArticle :: Lines s
makeOrgArticle = OrgArticle { time = Nothing, header = "", body = [], paper' = "" }
----------------------------------------------------------------------------------------------------
leapYear :: Integer -> Bool
leapYear y
  | y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y `mod` 4 == 0 = True
  | otherwise = False

monthEnd :: Integer -> Int -> Int
monthEnd year month
  | month == 1 ||
    month == 3 ||
    month == 5 ||
    month == 7 ||
    month == 8 ||
    month == 10 = 31
  | month == 4 ||
    month == 6 ||
    month == 9 ||
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

dateP :: Parser (Lines s)
dateP = do
  string "* "
  d <- manyTill (oneOf "0123456789/") eof
  return $ OrgDate d

lineP :: Parser (Lines s)
lineP = do
  s <- manyTill anyChar eof
  return $ OrgLine s

toLine :: String -> Lines s
toLine s = case parse selected "" s of
  Right s -> s
  Left _  -> OrgError
  where selected = choice [try titleP, try dateP, lineP]
----------------------------------------------------------------------------------------------------
type PaperMap = Map.Map Int [String]

papers :: [String]
papers = (++"新聞") <$> ["朝日", "毎日", "読売", "日経", "産経"]

takeHeader :: Lines s -> String -> Lines s
takeHeader art head' =
  art { header = head', paper' = p }
  where p = takePaper head'

takePaper :: String -> String
takePaper s = case parse takePaperParse mempty s of
  Right s' -> s'
  Left _   -> mempty

takePaperParse :: Parser String
takePaperParse = try inner <|> (anyChar >> takePaperParse)
  where inner = do
        char '['
        rel <- many1 (noneOf "]")
        char ']'
        return rel

makePaperMap :: [Lines s] -> PaperMap
makePaperMap = U.makeMap (timeToDay . time) paper'
  where timeToDay Nothing = 0
        timeToDay (Just d) = toDay d

paperElem :: Int -> PaperMap -> String -> Bool
paperElem day mp p = isJust $ elem p <$> Map.lookup day mp

notElemDayPaper :: [Int] -> [Lines s] -> [(Int, [String])]
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
dateFold :: [Lines s] -> [Lines s]
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
orgLineList :: String -> [Lines s]
orgLineList = dateFold . map toLine . lines

parseToDayList :: Integer -> Int -> IO [Day]
parseToDayList year month = do
  today    <- todayDay
  let file    = orgFileName year month
  let daylist = makeMonthList today year month
  contents <- orgLineList <$> U.readUTF8File file
  return $ map (fromGregorian year month) $ notElemDay daylist contents

-- testIO :: IO ()
testIO = do
  contents <- U.readUTF8File test
  I.hSetEncoding I.stdout I.utf8
  return $ orgLineList contents

