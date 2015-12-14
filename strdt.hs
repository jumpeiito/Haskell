{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Strdt (strdt, dtmap, Date, NendoDate,
              toYear, toYearInt, toMonth, toDay,
              howOld, nendoEnd
             ) where 

import Data.Time
import Data.List
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec

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

class StringDate a where
  strdt :: String -> a

instance StringDate (Maybe Day) where
  strdt str = _strdt str `dtmap` id

instance StringDate (Maybe String) where
  strdt str = _strdt str `dtmap` show

instance StringDate (Maybe Int) where
  strdt str =
    _strdt str `dtmap` (\n -> fromInteger $ toYear n :: Int)

instance StringDate (Maybe (Int, Int)) where
  strdt str =
    _strdt str `dtmap` (\d -> (toMonth d, toDay d))

instance StringDate (Maybe Date) where
  strdt str =
    _strdt str `dtmap` (\d -> (toYearInt d, toMonth d, toDay d))

instance StringDate (Maybe NendoDate) where
  strdt str =
    _strdt str `dtmap` returner
    where returner d = (nendoCalc d, toYearInt d, toMonth d, toDay d)
          nendoCalc d = (toYearInt d) - (sep d)
          sep d | (toMonth d) <= 3 = 1
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
date8 = do
  year  <- count 4 digit
  month <- count 2 digit
  day   <- count 2 digit
  return $ readDate year month day
  
date6 :: Parser Day
date6 = do
  year  <- count 4 digit
  month <- count 2 digit
  return $ readDate year month "1"

dateNormal :: Parser Day
dateNormal = do
  year  <- count 4 digit <* (oneOf $ separator ++ "年")
  month <- many digit    <* (oneOf $ separator ++ "月")
  day   <- (try $ count 2 digit) <|> (count 1 digit)
  return $ readDate year month day

dateJapaneseShort :: Parser Day
dateJapaneseShort = do
  gengou <- oneOf "MTSHmtsh明大昭平"
  y      <- many1 digit <* (oneOf $ separator ++ "年")
  month  <- many1 digit <* (oneOf $ separator ++ "月")
  day    <- many digit
  let y'   = read y :: Integer
      year = case [gengou] of
        g' | g' `isInfixOf` "Mm明" -> 1867 + y'
        g' | g' `isInfixOf` "Tt大" -> 1911 + y'
        g' | g' `isInfixOf` "Ss昭" -> 1925 + y'
        g' | g' `isInfixOf` "Hh平" -> 1988 + y'
        _ -> 1988 + y'
  return $ readDate (show year) month day
          
dateJapaneseLong :: Parser Day
dateJapaneseLong = do
  gengou <- choice [string "明治", string "大正", string "昭和", string "平成"]
  y      <- many1 digit <* (oneOf $ separator ++ "年")
  month  <- many1 digit <* (oneOf $ separator ++ "月")
  day    <- many digit
  let y'   = read y :: Integer
      year = case gengou of
        "明治" -> 1867 + y'
        "大正" -> 1911 + y'
        "昭和" -> 1925 + y'
        "平成" -> 1988 + y'
        _ -> 1988 + y'
  return $ readDate (show year) month day

calc :: Parser Day
calc = do
  try date8
  <|> try dateNormal
  <|> try dateJapaneseShort
  <|> try dateJapaneseLong
  <|> date6 

_strdt :: String -> Either ParseError Day
_strdt = parse calc ""

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

nendoEnd :: Integer -> Day
nendoEnd y = fromGregorian (y+1) 3 31
