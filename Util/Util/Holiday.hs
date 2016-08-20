module Util.Holiday where

import Data.Time
import Util.Strdt
import Data.Yaml.Parser

type Year  = Integer
type Month = Int
type DDay  = Int

data Holiday =
  Fix        (Month, DDay) String         -- 日付が固定している祝日
  | Abstract (Month, Int, DayWeek) String -- 「8月の第3週目の火曜日」 -> Abstract (8, 3, Tuesday)
  | Calc     (Year -> Day) String         -- 春分・秋分の日を年から計算する関数

holidays :: [Holiday]
holidays =
  [Fix      (1, 1)              "元旦",
   Abstract (1, 2, Monday)      "成人の日",
   Fix      (2, 11)             "建国記念日",
   Calc     vernalEquinox       "春分の日",
   Fix      (4, 29)             "昭和の日",
   Fix      (5, 3)              "憲法記念日",
   Fix      (5, 4)              "みどりの日",
   Fix      (5, 5)              "こどもの日",
   Abstract (7, 3, Monday)      "海の日",
   Fix      (8, 11)             "山の日",
   Abstract (9, 3, Monday)      "敬老の日",
   Calc     autumnalEquinox     "秋分の日",
   Abstract (10, 2, Monday)     "体育の日",
   Fix      (11, 3)             "文化の日",
   Fix      (11, 23)            "勤労感謝の日",
   Fix      (12, 23)            "天皇誕生日"]

makeHoliday :: Year -> [Day]
makeHoliday y = foldr (coref y) [] holidays
  where coref y' (Fix (m, d) _)          seed = fromGregorian y m d : seed
        coref y' (Abstract (m, d, dw) _) seed = calcDate y m d dw : seed
        coref y' (Calc f _)              seed = f y : seed

succeed :: (Enum a, Bounded a, Eq a) => a -> a
succeed a
  | a == maxBound = minBound
  | otherwise = succ a

calcDate :: Year -> Month -> Int -> DayWeek -> Day
calcDate y m nth dw = fromGregorian y m $ (nth - 1) * 7 + diff
  where monthTop   = fromGregorian y m 1
        monthTopDW = getWeekDate monthTop
        diff       = abs (fromEnum dw - fromEnum monthTopDW) + 1

yearInspect :: Year -> Integer -> Year -> Year -> Bool
yearInspect year modulo begin end =
  (year `mod` 4 == modulo) && (begin <= year) && (year <= end)

vernalEquinox :: Year -> Day
vernalEquinox y
  | yearInspect y 0 1900 1956 = fromGregorian y 3 21
  | yearInspect y 0 1960 2088 = fromGregorian y 3 20
  | yearInspect y 0 2092 2096 = fromGregorian y 3 19
  | yearInspect y 1 1901 1989 = fromGregorian y 3 21
  | yearInspect y 1 1993 2097 = fromGregorian y 3 20
  | yearInspect y 2 1902 2022 = fromGregorian y 3 21
  | yearInspect y 2 2026 2098 = fromGregorian y 3 20
  | yearInspect y 3 1903 1923 = fromGregorian y 3 22
  | yearInspect y 3 1927 2055 = fromGregorian y 3 21
  | yearInspect y 3 2059 2099 = fromGregorian y 3 20

autumnalEquinox :: Year -> Day
autumnalEquinox y
  | yearInspect y 0 1900 2008 = fromGregorian y 9 23
  | yearInspect y 0 2012 2096 = fromGregorian y 9 22
  | yearInspect y 1 1901 1917 = fromGregorian y 9 24 
  | yearInspect y 1 1921 2041 = fromGregorian y 9 23
  | yearInspect y 1 2045 2097 = fromGregorian y 9 22
  | yearInspect y 2 1902 1946 = fromGregorian y 9 24
  | yearInspect y 2 1950 2074 = fromGregorian y 9 23 
  | yearInspect y 2 2078 2098 = fromGregorian y 9 22
  | yearInspect y 3 1903 1979 = fromGregorian y 9 24
  | yearInspect y 3 1983 2099 = fromGregorian y 9 23
