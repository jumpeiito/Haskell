{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Strdt (strdt, dtmap, Date, NendoDate,
              toYear, toYearInt, toMonth, toDay, nendo,
              howOld, nendoEnd, today, todayDay, dayStr8, dayStrWithSep
             ) where 

import Data.Time
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Text.StringLike        as Like

type Date      = (Int, Int, Int)
type NendoDate = (Int, Int, Int, Int)
data WeekDate = Sunday
              | Monday
              | Tuesday
              | Wednesday
              | Thursday
              | Friday
              | Saturday
                deriving (Show, Eq, Ord)

dtmap :: Either a b -> (b -> c) -> Maybe c
(Right x) `dtmap` f = Just $ f x
(Left _)  `dtmap` _ = Nothing

class StringDate a b where
  strdt :: a -> b

instance Like.StringLike a => StringDate a (Maybe Day) where
  strdt str = _strdt str `dtmap` id

instance Like.StringLike a => StringDate a (Maybe String) where
  strdt str = _strdt str `dtmap` show

instance Like.StringLike a => StringDate a (Maybe Int) where
  strdt str =
    _strdt str `dtmap` (\n -> fromInteger $ toYear n :: Int)

instance Like.StringLike a => StringDate a (Maybe (Int, Int)) where
  strdt str =
    _strdt str `dtmap` (\d -> (toMonth d, toDay d))

instance Like.StringLike a => StringDate a (Maybe Date) where
  strdt str =
    _strdt str `dtmap` (\d -> (toYearInt d, toMonth d, toDay d))

instance Like.StringLike a => StringDate a (Maybe NendoDate) where
  strdt str =
    _strdt str `dtmap` returner
    where returner d = (nendoCalc d, toYearInt d, toMonth d, toDay d)
          nendoCalc d = toYearInt d - sep d
          sep d | toMonth d <= 3 = 1
                | otherwise = 0

separator :: String
separator = "./-"

readDate :: String -> String -> String -> Day
readDate y m d =
  let (year, month, day) = (read y :: Integer,
                            read m :: Int,
                            read d :: Int)
  in fromGregorian year month day

date8 :: Parser Day
date8 = readDate <$> count 4 digit
                 <*> count 2 digit
                 <*> count 2 digit
  
date6 :: Parser Day
date6 = readDate <$> count 4 digit
                 <*> count 2 digit
                 <*> return "1"

sepYear, sepMonth :: Parser Char
sepYear  = oneOf $ separator ++ "年"
sepMonth = oneOf $ separator ++ "月"

dateNormal :: Parser Day
dateNormal = readDate <$> count 4 digit <* sepYear
                      <*> many digit    <* sepMonth
                      <*> try (count 2 digit <|> count 1 digit)

gengouToYear :: (String, Integer) -> Integer
gengouToYear (g, y)
  | g `isInfixOf` "Mm明" || g == "明治" = 1867 + y
  | g `isInfixOf` "Tt大" || g == "大正" = 1911 + y
  | g `isInfixOf` "Ss昭" || g == "昭和" = 1925 + y
  | g `isInfixOf` "Hh平" || g == "平成" = 1988 + y
  | otherwise                          = 1988 + y

stringToGengouYear :: String -> String -> String
stringToGengouYear g y = show $ gengouToYear (g, read y :: Integer)

dateJapaneseShort :: Parser Day
dateJapaneseShort = readDate <$> (stringToGengouYear <$> g <*> y)
                             <*> many1 digit <* sepMonth
                             <*> many digit
  where g = (:[]) <$> oneOf "MTSHmtsh明大昭平"
        y = many1 digit <* sepYear

dateJapaneseLong :: Parser Day
dateJapaneseLong = readDate <$> (stringToGengouYear <$> g <*> y)
                            <*> many1 digit <* sepMonth
                            <*> many digit
  where g = choice [string "明治", string "大正", string "昭和", string "平成"]
        y = many1 digit <* sepYear

calc :: Parser Day
calc = 
  try date8
  <|> try dateNormal
  <|> try dateJapaneseShort
  <|> try dateJapaneseLong
  <|> date6 

_strdt :: Like.StringLike a => a -> Either ParseError Day
_strdt = parse calc "" . Like.castString

toYear :: Day -> Integer
toYear day = let (d, _, _) = toGregorian day in d

toYearInt :: Day -> Int
toYearInt day = (fromInteger $ toYear day) :: Int

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

dayStrWithSep :: Char -> Day -> String
dayStrWithSep c d = printf "%04d%c%02d%c%02d" y' c m' c d'
  where [y',m',d'] = [toYearInt, toMonth, toDay] <*> [d]

nendoEnd :: Integer -> Day
nendoEnd y = fromGregorian (y+1) 3 31

today :: IO (Integer, Int, Int)
today = do
  d <- todayDay
  return (toYear d, toMonth d, toDay d)

nendo :: Day -> Int
nendo d
  | month < 4 = year - 1
  | otherwise = year
  where month = toMonth d
        year  = fromInteger $ toYear d

-- todayDay :: IO Day
-- todayDay = do
--   (y, m, d) <- today
--   return (fromGregorian y m d)
todayDay :: IO Day
todayDay = do
  d   <- getCurrentTime
  let cur  = addUTCTime (9*60*60) d
  let dstr = formatTime defaultTimeLocale "%Y%m%d" cur
  return $ fromJust (strdt dstr :: Maybe Day)

-- gregorianDate :: CalendarTime -> (Integer, Int, Int)
-- gregorianDate cal = (fromIntegral $ ctYear cal, toNumber $ ctMonth cal, ctDay cal)

-- calendarNow :: IO CalendarTime
-- calendarNow = toCalendarTime =<< getClockTime

-- toNumber :: Month -> Int
-- toNumber January   = 1
-- toNumber February  = 2
-- toNumber March     = 3
-- toNumber April     = 4
-- toNumber May       = 5
-- toNumber June      = 6
-- toNumber July      = 7
-- toNumber August    = 8
-- toNumber September = 9
-- toNumber October   = 10
-- toNumber November  = 11
-- toNumber December  = 12
