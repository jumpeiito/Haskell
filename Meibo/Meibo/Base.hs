{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Meibo.Base ( Line (..)
                  , Key (..)
                  , deleteStr
                  , deleteStrMap
                  , trans
                  , meiboMain
                  , telephoneStr
                  , addressStr
                  , output
                  , getMeibo) where

import Codec.Xlsx                       hiding (Parser)
import Codec.Xlsx.Formatted
import qualified Data.ByteString.Lazy as L
import Control.Lens                     hiding (noneOf)
import Data.Maybe
import Data.Text                        (unpack, pack, Text)
import Data.Time.Calendar
import GHC.Float
import Util                             (runRubyString, readUTF8File, group)
import Util.Strdt
import Util.Telephone
import Util.StrEnum
import Data.List                        hiding (group)
import Data.Maybe                       (fromMaybe, isJust)
import Control.Monad.Writer
import Control.Monad.State
import Data.Time
import Control.Concurrent.Async
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
test2 :: Day -> [Text] -> Line
test2 day tx = 
  let [bnk, hn, sym, hcho, nm, ad', tel', exp', exp2', fu', bir] =
        map unpack tx
      adtel  = ad' ++ "・" ++ tel'
      telp   = telParse adtel
      birth' = strdt bir
  in Line { bunkai = bnk
          , bknum  = bunkaiNumber bnk
          , han    = hn
          , kind   = removeSymbol sym
          , hancho = case hcho of "" -> Nothing; _ -> Just "●"
          , gen    = adtel ++ "\n"
          , name   = nm
          , nameP  = nameParse nm
          , ad     = deleteStrMap remover adtel
          , tel    = telp
          , work   = exp'
          , furi   = fu'
          , birthS = bir
          , birth  = birth'
          , year   = howOld <$> birth' <*> Just day
          , Meibo.Base.exp = exp2' }

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
          
trans day tx = st2 day $ snd $ (`execState` ("", [])) $ ft2 tx

combinate :: [Text] -> [[Text]] -> [[Text]]
combinate txLine (car:cdr) = replaced : cdr
  where a `plus` b = mconcat [a, "・" , b]
        replaced   = zipWithNth [5,6] car txLine plus

ft2 :: [[Text]] -> State (Text, [[Text]]) [()]
ft2 csv = do
  forM csv $ \line -> do
    (hanNumber, ret) <- get
    case (line!!4=="", line!!1=="") of
      (True, _)  -> put (hanNumber, line `combinate` ret)
      (_, True)  -> put (hanNumber, changeHanNum hanNumber line : ret)
      (_, _)     -> put (line!!1, line : ret)
  where changeHanNum h (head':_:rest') = head':h:rest'
        changeHanNum _ _ = []

st2 :: Day -> [[Text]] -> [Line]
st2 day = map (test2 day)

meiboMain :: String -> IO [Line]
meiboMain bunkaiString = do
  (y, m, d) <- today
  output    <- getMeibo
  let currentDay = fromGregorian y m d
  let allList = trans currentDay output
  let filterF | bunkaiString == "全" = const True
              | otherwise = (\n -> bunkaiString == bunkai n)
  return $ filter filterF allList

telephoneStr :: Line -> String
telephoneStr l =
  Data.List.intercalate "," $ map telString $ tel l

addressStr :: Line -> String
addressStr l =
  take 20 str ++ postStr
  where str = deleteStrMap ["・", "･", ","] $ ad l
        postStr | length str <= 20 = ""
                | otherwise = "..."

outputBase :: Line -> String
outputBase l = 
  bun ++ han' ++ " " ++ name' ++ "\t" ++ telephoneStr l
  where bun   = (:[]) $ head $ bunkai l
        head' = take (2 - length (han l)) $ repeat '0'
        tail' = take (16 - length (name l)) $ repeat ' '
        name' = name l ++ tail'
        han'  = head' ++ han l

sexpSymbol :: (String, String) -> String
sexpSymbol (sym, val) = ":" ++ sym ++ " \"" ++ val ++ "\" "

sexpList :: [(String, String)] -> String
sexpList sxl = "(" ++ concatMap sexpSymbol sxl ++ ")"

output :: Line -> String
output l =
  sexpList [ ("value", outputBase l)
           , ("type", kind l)
           , ("hancho", bool)]
  where bool | isJust $ hancho l = "1"
             | otherwise = "0"

_toString :: CellValue -> Text
_toString (CellText x) = x
_toString (CellDouble d) = pack . show . truncate $ d
_toString (CellBool d) = pack $ show d
_toString (CellRich []) = ""
_toString (CellRich x) = mconcat $ map _richTextRunText x

toString :: Maybe CellValue -> Text
toString Nothing = ""
toString (Just x) = _toString x

getMeibo :: IO [[Text]]
getMeibo = do
  bs <- L.readFile "s:/馬場フォルダ/組合員名簿/組合員名簿.xlsm"
  let book  = toXlsx bs
  ms <- mapM (getMeiboSheet book) [ "石田"
                                  , "日野"
                                  , "小栗栖"
                                  , "一言寺"
                                  , "三宝院"
                                  , "点在" ]
  return $ concat ms
  

getMeiboSheet :: Xlsx -> Text -> IO [[Text]]
getMeiboSheet xlsx idx = do
  let flatten idx = [ xlsx ^? ixSheet idx . ixCell x . cellValue . _Just
                    | x <- (,) <$> [6..300] <*> [1..10]]
  let initial = group 10 (map toString $ flatten idx)
  return $ map (idx:) initial

