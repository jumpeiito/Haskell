{-# LANGUAGE FlexibleContexts #-}
module Meibo.Base ( Line (..)
                  , Key (..)
                  , deleteStr
                  , deleteStrMap
                  , trans
                  , firstTrans
                  , secondTrans
                  , trans) where

import Util.Strdt
import Util.Telephone
import Util.StrEnum
import Data.List
import Control.Monad.Writer
import Control.Monad.State
import Data.Time
import System.Process
import Text.Parsec                      hiding (Line, State)
import Text.Parsec.String
import qualified System.IO              as I

data Line = Line { bunkai :: String,
                   bknum  :: String,
                   han    :: String,
                   kind   :: String,
                   hancho :: Maybe String,
                   gen    :: String,
                   name   :: String,
                   nameP  :: (String, String),
                   ad     :: String,
                   tel    :: [Telephone],
                   work   :: String,
                   exp    :: String,
                   furi   :: String,
                   birthS :: String,
                   birth  :: Maybe Day,
                   year   :: Maybe Integer
                 } deriving (Show, Eq)

data Key =
  Bunkai String
  | Bk String
  | Han String
  | K String
  | Name String
  | Address String
  | Ftel String
  | Mtel String
  | Work String
  | Year String
  | Old (Maybe Day, Maybe Day)
  | Or  [Key]
  | And [Key]
  | Not Key deriving (Show, Eq, Read)
----------------------------------------------------------------------------------------------------
test :: Day -> Parser Line
test day = do
  bnk   <- choice (map string ["石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]) <* sep'
  hn    <- many1 digit <* sep'
  sym   <- cell <* sep'
  hcho  <- cell <* sep'
  nm    <- cell <* sep'
  ad'   <- cell <* sep'
  tel'  <- cell <* sep'
  exp'  <- cell <* sep'
  exp2' <- cell <* sep'
  fu'   <- cell <* sep'
  bir   <- cell
  let adtel  = ad' ++ "・" ++ tel'
  let telp   = telParse adtel
  let birth' = strdt bir
  return Line { bunkai = bnk,
                bknum  = bunkaiNumber bnk,
                han    = hn,
                kind   = removeSymbol sym,
                hancho = case hcho of "" -> Nothing; _ -> Just "●",
                gen    = adtel ++ "\n",
                name   = nm,
                nameP  = nameParse nm,
                ad     = deleteStrMap (map telString telp) adtel,
                tel    = telp,
                work   = exp',
                furi   = fu',
                birthS = bir,
                birth  = birth',
                year   = howOld <$> birth' <*> Just day,
                Meibo.Base.exp = exp2' }
    where sep  = ','
          cell = many $ noneOf [sep]
          sep' = char sep
          
removeSymbol :: String -> String
removeSymbol = deleteStrMap ["◎", "○"]

bunkaiNumber :: String -> String
bunkaiNumber s = case s of
  "石田"   -> "01"
  "日野"   -> "02"
  "小栗栖" -> "03"
  "一言寺" -> "04"
  "三宝院" -> "05"
  "点在"   -> "50"
  _        -> ""
----------------------------------------------------------------------------------------------------
_nameParse :: Parser (String, String)
_nameParse = (,) <$> (many1 (noneOf "　") <* char '　')
                 <*> many1 (noneOf "　")

nameParse :: String -> (String, String)
nameParse n = either (const ("", "")) id
                     $ parse _nameParse "" n
----------------------------------------------------------------------------------------------------
deleteStr :: String -> String -> String
deleteStr key target = snd $ runWriter (delStr key target)
  where (>>>) x = drop (length x)
        delStr :: String -> String -> Writer String String
        delStr _ [] = return []
        delStr key' target'@(t:ts)
          | key' `isPrefixOf` target' = delStr key' (key' >>> target')
          | otherwise               = do { tell [t]; delStr key' ts}
  
deleteStrMap :: [String] -> String -> String
deleteStrMap xs s = foldl (flip deleteStr) s xs
----------------------------------------------------------------------------------------------------
(<@>) :: String -> Int -> String
(<@>) str n = ',' `split` str !! n

blankP :: String -> Int -> Bool
blankP s n = (s <@> n) == ""

-- zipWithNth [2] ["a", "b", "c"] ["d", "e", "f"] (++)
-- -> ["a","b","cf"]
zipWithNth :: [Int] -> [a] -> [a] -> (a -> a -> a) -> [a]
zipWithNth numList a b f = snd $ runWriter (loop a b 0)
  where loop [] _ _ = tell mempty
        loop _ [] _ = tell mempty
        loop (x:xs) (y:ys) n
          | n `elem` numList = do { tell [f x y]; loop xs ys (n+1) }
          | otherwise        = do { tell [x];     loop xs ys (n+1) }
          
replaceConcat :: String -> [String] -> [String]
replaceConcat line (head:rest) =
  intercalate "," replaced : rest
  where (lineX, headX) = (split ',' line, split ',' head)
        a `plus` b     = a ++ "・" ++ b
        replaced       = zipWithNth [5,6] headX lineX plus

firstTrans :: [String] -> [String]
firstTrans lyne = reverse answer
  where (_, answer) = (`execState` ("0", [])) $ ftrans lyne

ftrans :: [String] -> State (String, [String]) [()]
ftrans csv = do
  forM csv $ \line -> do
    (hanNum, csvReturn) <- get
    let nameBlankP = line `blankP` 4
    let hanBlankP  = line `blankP` 1
    case (nameBlankP, hanBlankP) of
      (True, _) -> put (hanNum, line `replaceConcat` csvReturn)
      (_, True) -> put (hanNum, replace hanNum line : csvReturn)
      (_, _)    -> put (line <@> 1, line : csvReturn)
  where replace hNum lyne = case split ',' lyne of
                              head':_:rest' -> intercalate "," $ head':hNum:rest'
                              _             -> ""

secondTrans :: Day -> [String] -> [Line]
secondTrans _ [] = []
secondTrans day (x:xs) = case parse (test day) "" x of
  Right s -> s : secondTrans day xs
  Left _  -> secondTrans day xs

trans :: Day -> [String] -> [Line]
trans day = secondTrans day . firstTrans
