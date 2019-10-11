{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Util.Strbt where

import Control.Arrow
import Control.Applicative
import Data.Text (Text)
import Data.Time
import Util.KanParseB
import Data.Attoparsec.Text (count, satisfy , inClass, Parser, digit, try, choice, string, many1, parseOnly)
import Text.Printf
-- import qualified Data.Text as Tx

type Date      = (Int, Int, Int)
type NendoDate = (Int, Int, Int, Int)
-- data DayWeek = Monday
--              | Tuesday
--              | Wednesday
--              | Thursday
--              | Friday
--              | Saturday
--              | Sunday
--              deriving (Show, Eq, Ord, Bounded, Enum)

dtmap :: Either a b -> (b -> c) -> Maybe c
(Right x) `dtmap` f = Just $ f x
(Left _)  `dtmap` _ = Nothing

class StringDate a where
  strdt :: Text -> a

instance StringDate (Maybe Day) where
  strdt str = _strdt str `dtmap` id

instance StringDate (Maybe String) where
  strdt str = _strdt str `dtmap` show

instance StringDate (Maybe Int) where
  strdt str =
    _strdt str `dtmap` (fromInteger . toYear)

instance StringDate (Maybe (Int, Int)) where
  strdt str =
    _strdt str `dtmap` (toMonth &&& toDay)

instance StringDate (Maybe Date) where
  strdt str =
    _strdt str `dtmap` ((,,) <$> toYearInt <*> toMonth <*> toDay)

instance StringDate (Maybe NendoDate) where
  strdt str =
    _strdt str `dtmap` returner
    where returner  = (,,,) <$> nendoCalc <*> toYearInt <*> toMonth <*> toDay
          nendoCalc = (-) <$> toYearInt <*> sep
          sep d | toMonth d <= 3 = 1
                | otherwise = 0

separator, separatorY, separatorM :: String
separator = "./-"
separatorY = "./-年"
separatorM = "./-月"

-- readDate :: String -> String -> String -> Day
-- readDate y m d =
--   fromGregorian (read y) (read m) (read d)
-- ----------------------------------------------------------------------------------------------------
dateK, date8, date6, dateNormal, dateJapanese, calc :: Parser Day
dateK = fromGregorian
  <$> (stringToGengouYear <$> gengouParse <*> digitToInt 2)
  <*> digitToInt 2
  <*> digitToInt 2

digitToInt :: Read a => Int -> Parser a
digitToInt c = read <$> count c digit

date8 = fromGregorian <$> digitToInt 4
                      <*> digitToInt 2
                      <*> digitToInt 2

date6 = fromGregorian <$> digitToInt 4
                      <*> digitToInt 2
                      <*> return 1

dateNormal = fromGregorian
  <$> sepYear4
  <*> sepMonth
  <*> (read <$> (try (count 2 digit) <|> count 1 digit))

dateJapanese = fromGregorian
  <$> (stringToGengouYear <$> gengouParse <*> sepYear)
  <*> sepMonth
  <*> (try (read <$> many1 digit) <|> kanParseInt)

calc = do
  try dateK
  <|> try date8
  <|> try dateJapanese
  <|> try dateNormal
  <|> try date6
-- ----------------------------------------------------------------------------------------------------
oneOf :: String -> Parser Char
oneOf = satisfy . inClass
-- sepYear4, sepYear, sepMonth :: Parser String
kanParseInt :: Parser Int
kanParseInt = fromInteger <$> kanParse

sepYear4, sepYear :: Parser Integer
sepMonth :: Parser Int
sepYear4 = (digitToInt 4 <|> kanParse) <* oneOf separatorY
sepYear  = (read <$> many1 digit  <|> kanParse)
           <* oneOf separatorY
sepMonth = (read <$> many1 digit <|> kanParseInt)
           <* oneOf separatorM

gengouToYear :: (Int, Integer) -> Integer
gengouToYear (g, y)
  | g == 1    = 1867 + y
  | g == 2    = 1911 + y
  | g == 3    = 1925 + y
  | g == 4    = 1988 + y
  | g == 5    = 2018 + y
  | otherwise = 1988 + y

stringToGengouYear :: Int -> Integer -> Integer
stringToGengouYear g y = gengouToYear (g, y)

gengouParse :: Parser Int
gengouParse = do
  let meiji  = Prelude.map string ["明治", "M", "m", "明"]
  let taisho = Prelude.map string ["大正", "T", "t", "大"]
  let showa  = Prelude.map string ["昭和", "3", "S", "s", "昭"]
  let heisei = Prelude.map string ["平成", "4", "H", "h", "平"]
  let reiwa  = Prelude.map string ["令和", "5", "R", "h", "令"]
  (choice meiji  >> return 1)
    <|> (choice taisho >> return 2)
    <|> (choice showa  >> return 3)
    <|> (choice heisei >> return 4)
    <|> (choice reiwa  >> return 5)

_strdt :: Text -> Either String Day
_strdt = parseOnly calc

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

-- today :: IO (Integer, Int, Int)
-- today = f <$> todayDay
--   where f = (,,) <$> toYear <*> toMonth <*> toDay

nendo :: Day -> Int
nendo d
  | month < 4 = year - 1
  | otherwise = year
  where month = toMonth d
        year  = fromInteger $ toYear d

-- todayDay :: IO Day
-- todayDay = do
--   d   <- getCurrentTime
--   let cur  = addUTCTime (9*60*60) d
--   let dstr = formatTime defaultTimeLocale "%Y%m%d" cur
--   return $ fromJust (strdt dstr)

-- -- getWeekDate :: Day -> DayWeek
-- -- getWeekDate d = toEnum $ abs $ (1 -) $ getWeekDateInt d

-- -- getWeekDateInt :: Day -> Int
-- -- getWeekDateInt d = read [last $ showWeekDate d]

-- -- getWeekDateString :: Day -> String
-- -- getWeekDateString d = ["日", "月", "火", "水", "木", "金", "土"] !! getWeekDateInt d

-- -- japaneseDateString :: Day -> String
-- -- japaneseDateString d =
-- --   y' ++ "年" ++ m' ++ "月" ++ d' ++ "日(" ++ dw ++ ")"
-- --   where [y', m', d'] = map (\f -> show $ f d) [ fromInteger . toYear
-- --                                               , toMonth
-- --                                               , toDay]
-- --         dw' = getWeekDateInt d
-- --         dw  = ["日", "月", "火", "水", "木", "金", "土"] !! dw'

-- -- class DiffDate a b where
-- --   differ :: a -> a -> b

-- -- instance DiffDate Day Integer where
-- --   differ s1 s2 = 1 + abs (diffDays s1 s2)

-- -- instance DiffDate String Integer where
-- --   differ s1 s2 = either (const 0) (+1)
-- --                  $ abs <$> (diffDays <$> _strdt s1 <*> _strdt s2) 

-- -- instance DiffDate String [(DayWeek, Integer)] where
-- --   differ s1 s2 = toList $ makeCountMap id wlist
-- --     where diff  = fromInteger $ differ s1 s2
-- --           n     = fromMaybe 0 (getWeekDateInt <$> strdt s1)
-- --           wlist = take diff $ cycle (rotate (n - 1) [minBound..maxBound])

-- strdtSpec :: Spec
-- strdtSpec = do
--   let toD y m d = Just $ fromGregorian y m d
--   let strdtT = strdt . Tx.pack
--   describe "strdt test" $ do
--     it "7 digit chars, gengou(3 or 4)-year(2 digit)-month(2 digit)-day(2 digit)" $ do
--       (strdtT "3550714" :: Maybe Day)        `shouldBe` toD 1980 7 14
--       -- (strdtT "3550714" :: Maybe String)     `shouldBe` Just "1980-07-14"
--       -- (strdtT "3550714" :: Maybe Int)        `shouldBe` Just 1980
--       -- (strdtT "3550714" :: Maybe (Int, Int)) `shouldBe` Just (7, 14)
--       -- (strdtT "3550714" :: Maybe Date)       `shouldBe` Just (1980, 7, 14)
--       -- (strdtT "3550714" :: Maybe NendoDate)  `shouldBe` Just (1980, 1980, 7, 14)
--       (strdtT "4280201" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       -- (strdtT "4280201" :: Maybe String)     `shouldBe` Just "2016-02-01"
--       -- (strdtT "4280201" :: Maybe Int)        `shouldBe` Just 2016
--       -- (strdtT "4280201" :: Maybe (Int, Int)) `shouldBe` Just (2, 1)
--       -- (strdtT "4280201" :: Maybe Date)       `shouldBe` Just (2016, 2, 1)
--       -- (strdtT "4280201" :: Maybe NendoDate)  `shouldBe` Just (2015, 2016, 2, 1)
--     it "If it doesn't start 3 or 4, returns Nothing" $ do
--       (strdtT "5550714" :: Maybe Day)        `shouldBe` Nothing
--       (strdtT "0550714" :: Maybe Day)        `shouldBe` Nothing
--       -- to be fully implemented.
--       -- (strdt "4280001" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4280700" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4281301" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4270229" :: Maybe Day)        `shouldBe` Nothing
--       -- (strdt "4280229" :: Maybe Day)        `shouldBe` toD 2016 2 29
--       -- (strdt "4280931" :: Maybe Day)        `shouldBe` Nothing
--     it "8 digit chars, year(4 digit)-month(2 digit)-day(2 digit)" $ do
--       (strdtT "19800714" :: Maybe Day)        `shouldBe` toD 1980 7 14
--       -- (strdtT "19800714" :: Maybe String)     `shouldBe` Just "1980-07-14"
--       -- (strdtT "19800714" :: Maybe Int)        `shouldBe` Just 1980
--       -- (strdtT "19800714" :: Maybe (Int, Int)) `shouldBe` Just (7, 14)
--       -- (strdtT "19800714" :: Maybe Date)       `shouldBe` Just (1980, 7, 14)
--       -- (strdtT "19800714" :: Maybe NendoDate)  `shouldBe` Just (1980, 1980, 7, 14)
--       (strdtT "20160201" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       (strdtT "2016/02/01" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       (strdtT "2016/2/1" :: Maybe Day)        `shouldBe` toD 2016 2 1
--       -- (strdtT "20160201" :: Maybe String)     `shouldBe` Just "2016-02-01"
--       -- (strdtT "20160201" :: Maybe Int)        `shouldBe` Just 2016
--       -- (strdtT "20160201" :: Maybe (Int, Int)) `shouldBe` Just (2, 1)
--       -- (strdtT "20160201" :: Maybe Date)       `shouldBe` Just (2016, 2, 1)
--       -- (strdtT "20160201" :: Maybe NendoDate)  `shouldBe` Just (2015, 2016, 2, 1)

-- -- --     -- it "dateNormal-01"
-- -- --     -- it "dateNormal-02"
-- -- --     -- it "dateNormal-03"
-- -- --     -- it "dateNormal-04"
-- -- --     -- it "dateNormal-05"
-- -- --     -- it "dateJapanese-01"
-- -- --     -- it "dateJapanese-02"
-- -- --     -- it "dateJapanese-03"
-- -- --     -- it "dateJapanese-04"
-- -- --     -- it "dateJapanese-05"
-- -- --     -- it "dateJapanese-06"
-- -- --     -- it "date6-01"
-- -- --     -- it "date6-02"
-- -- --     -- it "date6-03"
-- -- --     -- it "date6-04"
-- -- --     -- it "date6-05"
    
