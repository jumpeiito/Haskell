module OrgQuery (Lines (..), Tag (..), Org (..), Query (..), tagList, makeQuery, askQuery) where

import Data.Time
import Util

type Title    = String
type Paper    = String
type Priority = String
type Tags     = [String]

data Lines =
  OrgDate Day
  | Header Title Paper Priority Tag
  | Blank
  | Line String deriving Show

data Tag = T [String] | Nop deriving Show

data Org = Org { number   :: Int,
                 date     :: Data.Time.Day,
                 paper    :: String,
                 priority :: String,
                 title    :: String,
                 tag      :: Tag,
                 list     :: [Lines]
               } deriving Show

data Query = Date String
             | DateRange String String
             | Tags [String]
             | Regexp [String]
             | Paper String
             | Priority String deriving Show

str2Date :: String -> Day
str2Date date =
  fromGregorian y m d
  where y':m:d:_ = map (\m -> read m :: Int) $ split '/' date
        y = toInteger y'

dateRegexp :: String
dateRegexp = "[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}"

paperP :: String -> (Org -> Bool)
paperP str = (\org -> paper org == str)

priorityP :: String -> (Org -> Bool)
priorityP str = (\org -> priority org == str)

dateP :: String -> (Org -> Bool)
dateP str = (\org -> date org == str2Date str)

dateRangeP :: (String, String) -> (Org -> Bool)
dateRangeP (st, en) =
  (\ org ->
     let d = date org in
     st' <= d && d <= en')
  where st' = str2Date st
        en' = str2Date en

hasTag :: Org -> String -> Bool
hasTag org queryTag =
  case tag org of
  Nop       -> False
  T taglist -> queryTag `elem` taglist

tagP :: [String] -> (Org -> Bool)
tagP strList = (\ org -> any (hasTag org) strList)

tagList :: Org -> [String]
tagList org =
  case tag org of
  Nop -> []
  T s -> s

-- hasRegex :: Org -> [String] -> Bool
-- hasRegex org [] = False
-- hasRegex org regexes =
  

makeQueryFunction :: Query -> (Org -> Bool)
makeQueryFunction q =
  case q of
  Date s          -> dateP s
  DateRange s1 s2 -> dateRangeP(s1, s2)
  Tags s          -> tagP s
  Paper s         -> paperP s
  Priority s      -> priorityP s

makeQuery :: [Query] -> [(Org -> Bool)]
makeQuery [] = []
makeQuery (x:xs) =
  (makeQueryFunction x):(makeQuery xs)

translateQuery :: String -> [Query]
translateQuery str =
  map translate strList
  where strList = split '|' str
        translate subs =
          let key:val:_ = split '=' subs in
          case key of
          "date"     -> Date val
          "range"    -> DateRange s1 s2
                        where s1:s2:_ = split ',' val
          "tags"     -> Tags $ split ',' val
          "paper"    -> Paper val
          "priority" -> Priority val

askQuery :: String -> Org -> Bool
askQuery qstr o =
  loop $ makeQuery $ translateQuery qstr
  where loop [] = True
        loop (f:fs) = if f o then loop fs else False
