{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Util.Strdt (strdt
                  , dtmap
                  , Date
                  , NendoDate
                  , DayWeek (..)
                  , toYear
                  , toYearInt
                  , toMonth
                  , toDay
                  , nendo
                  , howOld
                  , nendoEnd
                  , today
                  , todayDay
                  , dayStr8
                  , dayStr6
                  , dayStrWithSep
                  , getWeekDate
                  , getWeekDateInt
                  , getWeekDateString
                  , japaneseDateString) where

import Util           hiding ((&&&))
import Control.Arrow
import Data.List
import Data.Map                         hiding (map)
import Data.Maybe
import Data.Time
import Data.Time.Calendar.WeekDate
import Util.KanParse
import Text.ParserCombinators.Parsec
import Text.Printf
import Text.StringLike                  (StringLike, castString)
-- import Test.Hspec

type Date      = (Int, Int, Int)
type NendoDate = (Int, Int, Int, Int)
data DayWeek = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show, Eq, Ord, Bounded, Enum)

dtmap :: Either a b -> (b -> c) -> Maybe c
(Right x) `dtmap` f = Just $ f x
(Left _)  `dtmap` _ = Nothing

class StringDate a b where
  strdt :: a -> b

instance StringLike a => StringDate a (Maybe Day) where
  strdt str = _strdt str `dtmap` id

instance StringLike a => StringDate a (Maybe String) where
  strdt str = _strdt str `dtmap` show

instance StringLike a => StringDate a (Maybe Int) where
  strdt str =
    _strdt str `dtmap` (fromInteger . toYear)

instance StringLike a => StringDate a (Maybe (Int, Int)) where
  strdt str =
    _strdt str `dtmap` (toMonth &&& toDay)

instance StringLike a => StringDate a (Maybe Date) where
  strdt str =
    _strdt str `dtmap` ((,,) <$> toYearInt <*> toMonth <*> toDay)

instance StringLike a => StringDate a (Maybe NendoDate) where
  strdt str =
    _strdt str `dtmap` returner
    where returner  = (,,,) <$> nendoCalc <*> toYearInt <*> toMonth <*> toDay
          nendoCalc = (-) <$> toYearInt <*> sep
          sep d | toMonth d <= 3 = 1
                | otherwise = 0

separator :: String
separator = "./-"

readDate :: String -> String -> String -> Day
readDate y m d =
  fromGregorian (read y) (read m) (read d)
----------------------------------------------------------------------------------------------------
dateK, date8, date6, dateNormal, dateJapanese, calc :: Parser Day
dateK = readDate <$> (stringToGengouYear <$> gengouParse <*> count 2 digit)
                 <*> count 2 digit
                 <*> count 2 digit

date8 = readDate <$> count 4 digit
                 <*> count 2 digit
                 <*> count 2 digit

date6 = readDate <$> count 4 digit
                 <*> count 2 digit
                 <*> return "1"

dateNormal = readDate <$> sepYear4
                      <*> sepMonth
                      <*> (try (count 2 digit) <|> count 1 digit)

dateJapanese = readDate <$> (stringToGengouYear <$> gengouParse <*> sepYear)
                        <*> sepMonth
                        <*> (try (many1 digit) <|> kanParseStr)

calc = do
  try dateK
  <|> try date8
  <|> try dateNormal
  <|> try dateJapanese
  <|> try date6 
----------------------------------------------------------------------------------------------------
sepYear4, sepYear, sepMonth :: Parser String
sepYear4 = (count 4 digit <|> kanParseStr) <* oneOf (separator ++ "年")
sepYear  = (many1 digit   <|> kanParseStr) <* oneOf (separator ++ "年")
sepMonth = (many1 digit   <|> kanParseStr) <* oneOf (separator ++ "月")

gengouToYear :: (String, Integer) -> Integer
gengouToYear (g, y)
  | g `isInfixOf` "Mm明" || g == "明治" = 1867 + y
  | g `isInfixOf` "Tt大" || g == "大正" = 1911 + y
  | g `isInfixOf` "Ss昭" || g == "昭和" || g == "3" = 1925 + y
  | g `isInfixOf` "Hh平" || g == "平成" || g == "4" = 1988 + y
  | otherwise                          = 1988 + y

stringToGengouYear :: String -> String -> String
stringToGengouYear g y = show $ gengouToYear (g, read y)

gengouParse :: Parser String
gengouParse =
  try (choice [ string "明治", string "大正", string "昭和", string "平成"
              , string "3", string "4" ])
  <|> (:[]) <$> oneOf "MTSHmtsh明大昭平"

_strdt :: StringLike a => a -> Either ParseError Day
_strdt = parse calc "" . castString

toYear :: Day -> Integer
toYear day = let (d, _, _) = toGregorian day in d

toYearInt :: Day -> Int
toYearInt day = fromInteger $ toYear day

toMonth :: Day -> Int
toMonth day = let (_, m, _) = toGregorian day in m

toDay :: Day -> Int
toDay day = let (_, _, d) = toGregorian day in d

howOld :: Day -> Day -> Integer
howOld from to =
  y'' - y' + gap
  where (y', m', d') = toGregorian from
        (y'', m'', d'') = toGregorian to
        gap = if (m'' * 100 + d'') - (m' * 100 + d') > 0 then 0 else -1

dayStr8 :: Day -> String
dayStr8 d = printf "%04d%02d%02d" y' m' d'
  where [y',m',d'] = [toYearInt, toMonth, toDay] <*> [d]

dayStr6 :: Day -> String
dayStr6 d = printf "%04d%02d" y' m'
  where [y',m'] = [toYearInt, toMonth] <*> [d]

dayStrWithSep :: Char -> Day -> String
dayStrWithSep c d = printf "%04d%c%02d%c%02d" y' c m' c d'
  where [y',m',d'] = [toYearInt, toMonth, toDay] <*> [d]

nendoEnd :: Integer -> Day
nendoEnd y = fromGregorian (y+1) 3 31

today :: IO (Integer, Int, Int)
today = f <$> todayDay
  where f = (,,) <$> toYear <*> toMonth <*> toDay

nendo :: Day -> Int
nendo d
  | month < 4 = year - 1
  | otherwise = year
  where month = toMonth d
        year  = fromInteger $ toYear d

todayDay :: IO Day
todayDay = do
  d   <- getCurrentTime
  let cur  = addUTCTime (9*60*60) d
  let dstr = formatTime defaultTimeLocale "%Y%m%d" cur
  return $ fromJust (strdt dstr)

getWeekDate :: Day -> DayWeek
getWeekDate d = toEnum $ abs $ (1 -) $ getWeekDateInt d

getWeekDateInt :: Day -> Int
getWeekDateInt d = read [last $ showWeekDate d]

getWeekDateString :: Day -> String
getWeekDateString d = ["日", "月", "火", "水", "木", "金", "土"] !! getWeekDateInt d

japaneseDateString :: Day -> String
japaneseDateString d =
  y' ++ "年" ++ m' ++ "月" ++ d' ++ "日(" ++ dw ++ ")"
  where [y', m', d'] = map (\f -> show $ f d) [ fromInteger . toYear
                                              , toMonth
                                              , toDay]
        dw' = getWeekDateInt d
        dw  = ["日", "月", "火", "水", "木", "金", "土"] !! dw'

class DiffDate a b where
  differ :: a -> a -> b

instance DiffDate Day Integer where
  differ s1 s2 = 1 + abs (diffDays s1 s2)

instance DiffDate String Integer where
  differ s1 s2 = either (const 0) (+1)
                 $ abs <$> (diffDays <$> _strdt s1 <*> _strdt s2) 

instance DiffDate String [(DayWeek, Integer)] where
  differ s1 s2 = toList $ makeCountMap id wlist
    where diff  = fromInteger $ differ s1 s2
          n     = fromMaybe 0 (getWeekDateInt <$> strdt s1)
          wlist = take diff $ cycle (rotate (n - 1) [minBound..maxBound])

-- strdtSpec :: Spec
-- strdtSpec = do
--   let toD y m d = Just $ fromGregorian y m d
--   describe "strdt test" $ do
--     it "7 digit chars, gengou(3 or 4)-year(2 digit)-month(2 digit)-day(2 digit)" $ do
--       (strdt "3550714" :: Maybe Day)        `shouldBe` toD 1980 7 14
--       (strdt "3550714" :: Maybe String)     `shouldBe` Just "1980-07-14"
--       (strdt "3550714" :: Maybe Int)        `shouldBe` Just 1980
--       (strdt "3550714" :: Maybe (Int, Int)) `shouldBe` Just (7, 14)
--       (strdt "3550714" :: Maybe Date)       `shouldBe` Just (1980, 7, 14)
--       (strdt "3550714" :: Maybe NendoDate)  `shouldBe` Just (1980, 1980, 7, 14)
--       (strdt "4280201" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       (strdt "4280201" :: Maybe String)     `shouldBe` Just "2016-02-01"
--       (strdt "4280201" :: Maybe Int)        `shouldBe` Just 2016
--       (strdt "4280201" :: Maybe (Int, Int)) `shouldBe` Just (2, 1)
--       (strdt "4280201" :: Maybe Date)       `shouldBe` Just (2016, 2, 1)
--       (strdt "4280201" :: Maybe NendoDate)  `shouldBe` Just (2015, 2016, 2, 1)
--     it "If it doesn't start 3 or 4, returns Nothing" $ do
--       (strdt "5550714" :: Maybe Day)        `shouldBe` Nothing
--       (strdt "0550714" :: Maybe Day)        `shouldBe` Nothing
--       -- to be fully implemented.
--       -- (strdt "4280001" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4280700" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4281301" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4270229" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4280229" :: Maybe Day)        `shouldBe` toD 2016 2 29
--       -- (strdt "4280931" :: Maybe Day)        `shouldBe` Nothing
--     it "8 digit chars, year(4 digit)-month(2 digit)-day(2 digit)" $ do
--       (strdt "19800714" :: Maybe Day)        `shouldBe` toD 1980 7 14
--       (strdt "19800714" :: Maybe String)     `shouldBe` Just "1980-07-14"
--       (strdt "19800714" :: Maybe Int)        `shouldBe` Just 1980
--       (strdt "19800714" :: Maybe (Int, Int)) `shouldBe` Just (7, 14)
--       (strdt "19800714" :: Maybe Date)       `shouldBe` Just (1980, 7, 14)
--       (strdt "19800714" :: Maybe NendoDate)  `shouldBe` Just (1980, 1980, 7, 14)
--       (strdt "20160201" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       (strdt "20160201" :: Maybe String)     `shouldBe` Just "2016-02-01"
--       (strdt "20160201" :: Maybe Int)        `shouldBe` Just 2016
--       (strdt "20160201" :: Maybe (Int, Int)) `shouldBe` Just (2, 1)
--       (strdt "20160201" :: Maybe Date)       `shouldBe` Just (2016, 2, 1)
--       (strdt "20160201" :: Maybe NendoDate)  `shouldBe` Just (2015, 2016, 2, 1)

-- --     -- it "dateNormal-01"
-- --     -- it "dateNormal-02"
-- --     -- it "dateNormal-03"
-- --     -- it "dateNormal-04"
-- --     -- it "dateNormal-05"
-- --     -- it "dateJapanese-01"
-- --     -- it "dateJapanese-02"
-- --     -- it "dateJapanese-03"
-- --     -- it "dateJapanese-04"
-- --     -- it "dateJapanese-05"
-- --     -- it "dateJapanese-06"
-- --     -- it "date6-01"
-- --     -- it "date6-02"
-- --     -- it "date6-03"
-- --     -- it "date6-04"
-- --     -- it "date6-05"
    
