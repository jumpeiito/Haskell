{-# LANGUAGE FlexibleContexts #-}
module Main where

import Util                             hiding ((&&&))
import Util.Strdt
import Util.Telephone
import Util.StrEnum
import Data.Time
import Data.List
import Data.Maybe                       (isJust, fromJust, fromMaybe)
import Meibo.Base                       (Line (..), Key (..), deleteStr, deleteStrMap, trans, getMeibo, meiboMain)
import Text.Parsec                      hiding (Line, State)
import Text.Parsec.String
import Control.Arrow                    ((&&&))
import Control.Monad.Writer
import Control.Monad.State
import System.Environment
import qualified Data.Map               as Map
import qualified System.IO              as I
        
fixTel, mobileTel :: Line -> [Telephone]
fixTel = fixFilter . tel
mobileTel = mobileFilter . tel

mobileBlankP :: Line -> Bool
mobileBlankP = null . mobileTel

addressMap :: [Line] -> Map.Map String [Line]
addressMap = makeMap ad id 

testFilter :: [Line] -> [Line]
testFilter lines' = filter hofoo lines'
  where hofoo lyne = case (key lyne, mobileBlankP lyne) of
          (Just _, True) -> True
          _              -> False
        mapp  = addressMap lines'
        key l = Map.lookup (ad l) mapp

lineToTel :: [Telephone] -> String
lineToTel = intercalate "・" . map telString

lineMobile, lineFix :: Line -> String
lineMobile = lineToTel . mobileTel
lineFix    = lineToTel . fixTel

functionsToString :: [Line -> String] -> Line -> String
functionsToString fs l = intercalate "," $ map (\f -> f l) fs

mainString :: Line -> String
mainString =
  functionsToString [bunkai
                    , han
                    , fromMaybe "" . hancho
                    , kind
                    , name
                    , furi
                    , blank
                    , ad'
                    , lineMobile
                    , lineFix
                    , work
                    , Meibo.Base.exp
                    -- , show . fromMaybe (fromGregorian 1900 1 1)  . birth
                    , show . fromMaybe 0 . year
                    , name
                    ]
  where ad' line = deleteStrMap [".", "・", "･", " ", "　"] $ ad line
        blank _           = ""
----------------------------------------------------------------------------------------------------
hanchoMap :: [Line] -> Map.Map Int [Line]
hanchoMap = makeMap f id
  where f line' = (100 * read (bknum line')) + read (han line')
        
hanchoFilter :: [Line] -> [Line]
hanchoFilter = filter (isJust . hancho)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

(-->) :: Maybe t -> (t -> String) -> String
x --> f = fromMaybe "" (f <$> x)

hanchoList :: (Int, [Line]) -> [String]
hanchoList (bkHanCode, v) = [bkn, bnk, hn, name', fam', len]
  where hncho          = safeHead $ hanchoFilter v
        name'          = hncho --> name
        bnk            = hncho --> bunkai
        fam'           = hncho --> (fst . nameP)
        len            = show $ length v
        bkn            = show bcode
        hn             = show hcode
        (bcode, hcode) = ((`div` 100) &&& (`mod` 100)) bkHanCode

hanInfo :: (Int, [Line]) -> String
hanInfo = intercalate "," . hanchoList

hanDay :: [String] -> String -> Maybe String
hanDay lis bk
  | bk == "" || bk == "50" = Nothing
  | otherwise  = case lis!!(read bk - 1) of
    "_" -> Nothing
    x   -> Just x

hanOutput :: (Int, [String]) -> Int -> (Int, [Line]) -> String
hanOutput (month, day) today' = opfunc . hanchoList
  where arguments (bn:_:h:_:n:l:_) =
          intercalate "}{" [h, l, n, m bn, d bn]
        arguments _ = ""
        d bn = fromMaybe "" $ hanDay day bn
        m bn | d bn == "" = ""
             | otherwise = if today' > read (d bn)
                           then show $ month + 1
                           else show month

        opfunc list = 
          "\\hancholine{" ++ arguments list ++ "}"
----------------------------------------------------------------------------------------------------
makeTable :: Integer -> [(String, Day, Day)]
makeTable y' =
  [("見", (fG (y'-21) 4 2), (fG 3000 4 1)),
   ("青", (fG (y'-26) 4 2), (fG (y'-21) 4 1)),
   ("一", (fG (y'-70) 4 2), (fG (y'-26) 4 1)),
   ("老", (fG 1900 4 2), (fG (y'-70) 4 1))]
  where fG = fromGregorian

betweenP :: Ord a => (a, a) -> a -> Bool
betweenP (start, end) key
  | start < key && key < end = True
  | otherwise = False

betweenP2 :: Ord a => (a, a) -> a -> Bool
betweenP2 (start, end) key
  | start <= key && key <= end = True
  | otherwise = False

fromBirthday :: [(String, Day, Day)] -> Day -> String
fromBirthday table d = coref table
  where coref [] = ""
        coref ((k, start, end):xs)
          | betweenP2 (start, end) d = k
          | otherwise = coref xs

checK :: [(String, Day, Day)] -> Line -> Bool
checK table l = kind l == kind2 l
  where kind2 line = fromJust $ fromBirthday table <$> birth line

checkFilter :: [(String, Day, Day)] -> [Line] -> [Line]
checkFilter table = filter (not . checK table)
----------------------------------------------------------------------------------------------------
translateFunc, translateFuncPartial :: (Line -> String) -> String -> Line -> Bool
translateFunc func key n = key == func n

translateFuncPartial func key n = key `isInfixOf` func n

translateFuncTel :: (Line -> [Telephone]) -> String -> Line -> Bool
translateFuncTel func key n = key `isInfixOf` telpn n
  where telpn = intercalate "," . map telString . func

translateFuncYear :: String -> Line -> Bool
translateFuncYear y n = read y == fromMaybe 0 (year n)

translateFuncOld :: (Maybe Day, Maybe Day) -> Line -> Bool
translateFuncOld (start, end) n = fromJust $ (&&) <$> startBool n <*> endBool n
  where startBool n' = (>=) <$> birth n' <*> start
        endBool n'   = (<=) <$> birth n' <*> end

translateFold :: (Bool -> Bool -> Bool) -> Bool -> [Key] -> (Line -> Bool)
translateFold func bool list line =
  foldl func bool $ map (`translate` line) list

translate :: Key -> Line -> Bool
translate (Or  list)  = translateFold (||) False list
translate (And list)  = translateFold (&&) True list
translate (Not term)  = not . translate term
translate (Bunkai s)  = translateFunc bunkai s
translate (Bk i)      = translateFunc bknum i
translate (Han i)     = translateFunc han i
translate (K i)       = translateFuncPartial kind i
translate (Name s)    = translateFuncPartial name s
translate (Address s) = translateFuncPartial ad s
translate (Ftel s)    = translateFuncTel fixTel s
translate (Mtel s)    = translateFuncTel mobileTel s
translate (Work s)    = translateFuncPartial work s
translate (Year s)    = translateFuncYear s
translate (Old s)     = translateFuncOld s

seek :: Key -> [Line] -> [Line]
seek term = filter (translate term) 
----------------------------------------------------------------------------------------------------
keyParseLine :: Parser [Key]
keyParseLine = sepBy keyParseTerm $ char ','

keyParseBuilder :: (Char, Char) -> Parser a -> (a -> Key) -> Parser Key
keyParseBuilder (s, e) f op = op <$> between (char s) (char e) f

aChar :: Parser Char
aChar = noneOf "=,)]>"

keyParseOr, keyParseAnd, keyParseNot, keyParseTermWord, keyParseTerm :: Parser Key

keyParseOr  = keyParseBuilder ('[', ']') keyParseLine Or
keyParseAnd = keyParseBuilder ('(', ')') keyParseLine And
keyParseNot = keyParseBuilder ('<', '>') keyParseTerm Not

bkhanCodeParse :: Parser Key
bkhanCodeParse = do
  bk'  <- count 2 digit
  han' <- many digit
  return $ And [Bk bk', Han han']

yearoldParse :: Parser Key
yearoldParse = do
  let ac = noneOf "=,)]>&"
  start <- many1 ac <* char '&'
  end   <- many1 ac
  let s = strdt start
  let e = strdt end
  return $ Old (s, e)

returnParse :: Parser Key -> String -> Key
returnParse parser code' = either (const $ And []) id $ parse parser "" code'

bkhanCode :: String -> Key
bkhanCode = returnParse bkhanCodeParse

yearOld :: String -> Key
yearOld = returnParse yearoldParse

keyParseTermWord = do
  key' <- many1 aChar <* char '='
  val' <- many1 aChar
  case key' of
    "bunkai" -> return $ Bunkai val'
    "bk"     -> return $ Bk val'
    "han"    -> return $ Han val'
    "k"      -> return $ K val'
    "name"   -> return $ Name val'
    "ad"     -> return $ Address val'
    "ftel"   -> return $ Ftel val'
    "mtel"   -> return $ Mtel val'
    "work"   -> return $ Work val'
    "year"   -> return $ Year val'
    "old"    -> return $ yearOld val'
    "c"      -> return $ bkhanCode val'
    _        -> return $ And []

keyParseTerm =
  try keyParseOr
  <|> try keyParseAnd
  <|> try keyParseNot
  <|> try keyParseTermWord

keyParse :: String -> Key
keyParse str = either (const $ And []) head
                      $ parse keyParseLine "" str

seekS :: String -> [Line] -> [Line]
seekS str = seek (keyParse str)
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  locEncoding

  (y', m', d')    <- today
  -- _               <- runRubyString ["f:/Haskell/Meibo/meibo.rb"]
  -- commandOutput   <- lines <$> readUTF8File "f:/Haskell/Meibo/.meibo"
  commandOutput <- getMeibo
  let currentDay = fromGregorian y' m' d'
  let mainList = trans currentDay commandOutput
  args            <- getArgs
----------------------------------------------------------------------------------------------------
  case args of
    ["seek", key] -> mainOut $ seekS key mainList
    []            -> mainOut mainList
    ["check"]     -> let table = makeTable y' in (mainOut $ checkFilter table mainList)
    ["hch"]       -> printer hanInfo $ mapToList mainList
    "hchp":l      -> printer (hanOutput (m' , l) d') $ mapToList mainList
    _             -> putStrLn ""
----------------------------------------------------------------------------------------------------
  where printer f = mapM_ (putStrLn . f)
        mainOut   = printer mainString
        mapToList = Map.assocs . hanchoMap
