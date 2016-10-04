module Kensin.Base ( Status (..)
                   , Kind (..)
                   , Bunkai (..)
                   , KensinData (..)
                   , KensinBool
                   , CfgReader
                   , CfgReaderT
                   , KensinPrice
                   , KParse
                   , Option
                   , Translator
                   , strToBunkai
                   , bunkaiToStr
                   , lineToData
                   , latexCommand
                   , latexEnvironment
                   , splitSundayOrNot
                   , hasAmount
                   , toTime
                   , (==>)) where

import Util.StrEnum                     (split)
import Util.Strdt                       (howOld, nendoEnd, strdt)
import Data.Time
import Data.Array                       ((!), listArray)
import Data.Maybe                       (fromMaybe)
import Data.Either                      (rights)
import Control.Monad.Reader
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Read                        (readEither)
import Kensin.Config
import qualified Text.Printf            as TP

data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data Bunkai = Ishida
  | Hino
  | Ogurisu
  | Ichigonji
  | Sampoin
  | Tenzai deriving (Show, Ord, Eq, Bounded, Enum)
data KensinData = KensinData { day      :: Day
                             , sortKey  :: Maybe String
                             , name     :: String
                             , furigana :: String
                             , gender   :: Gender
                             , old      :: Integer
                             , bunkai   :: Bunkai
                             , number   :: Maybe String
                             , kind     :: Kind
                             , stat     :: Status
                             , kday     :: String
                             , amount   :: KParse Integer
                             , key      :: KParse (Day, Integer, Integer)
                             , pay      :: KParse [String]
                             , nonPay   :: KParse [String] } deriving (Show, Eq)

instance Ord KensinData where
  compare (KensinData _ x _ _ _ _ _ _ _ _ _ _ _ _ _) (KensinData _ y _ _ _ _ _ _ _ _ _ _ _ _ _)
    | x > y = GT
    | x == y = EQ
    | otherwise = LT
--alias---------------------------------------------------------------------------------------------
type KensinBool  = KensinData -> Bool
type CfgReader   = Reader Config
type CfgReaderT  = ReaderT Config IO
type KensinPrice = (String, Integer, Integer)
type KParse      = Either ParseError
type Option      = [String]
type Translator  = KensinData -> String
----------------------------------------------------------------------------------------------------
strToBunkai :: String -> Bunkai
strToBunkai str | str == "石田"    = Ishida
                | str == "日野"    = Hino
                | str == "小栗栖"  = Ogurisu
                | str == "一言寺"  = Ichigonji
                | str == "三宝院"  = Sampoin
                | str == "点在"    = Tenzai

bunkaiToStr :: Bunkai -> String
bunkaiToStr bk = (`runReader` config) $ do
  ary <- bkArray <$> ask
  return $ ary ! fromEnum bk

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

(==>) :: (String, String) -> (a, a) -> a
(sym, target) ==> (yes, no)
  | sym == target = yes
  | otherwise     = no


lineToData :: [String] -> CfgReader KensinData
lineToData line = do
  nendo' <- year <$> ask
  let old' = fromMaybe 0 $ (`howOld` nendoEnd nendo') <$> strdt birth
  return KensinData { day       = realday
                    , kday      = kday'
                    , Kensin.Base.name = n
                    , furigana  = furi
                    , gender    = g'
                    , old       = old'
                    , kind      = kind'
                    , bunkai    = strToBunkai bk
                    , number    = number'
                    , stat      = stat'
                    , key       = key'
                    , amount    = makeAmount stat' old' <$> pay'
                    , nonPay    = toPay nop d
                    , pay       = pay'
                    , sortKey   = genSortKey key' }
  where [bk, n, furi, g, birth, num, k, st, day', kday', nop, op] = extractElement line `runReader` config
        key'       = toKey day'
        d          = fst3 <$> key'
        number'    = case num of ""   -> Nothing; s -> Just s
        g'         = (g,  "男") ==> (Male, Female)
        stat'      = (st, "1")  ==> (Already, Yet)
        kind'      = (k,  "本") ==> (H, K)
        pay'       = toPay op d
        realday    = either (const (fromGregorian 1900 1 1)) id d
        
toKeyParse :: Parser (Day, Integer, Integer)
toKeyParse = do
  year'   <- read <$> count 4 digit <* char '-'
  month'  <- read <$> count 2 digit <* char '-'
  day'    <- read <$> count 2 digit <* char ' '
  hour'   <- read <$> count 2 digit <* char ':'
  minute' <- read <$> count 2 digit <* many anyChar
  return (fromGregorian year' month' day', hour', minute')

toKey :: String -> KParse (Day, Integer, Integer)
toKey = parse toKeyParse ""

latexCommand :: String -> [String] -> String
latexCommand com args = "\\" ++ com ++ concatMap enclose args
  where enclose str = "{" ++ str ++ "}"

latexEnvironment :: String -> Maybe String -> String -> String
latexEnvironment name option inner = 
  "\\begin{" ++ name ++ "}" ++ optionStr ++
  inner ++ 
  "\\end{" ++ name ++ "}"
  where optionStr = maybe "" (\n -> "{" ++ n ++ "}") option

splitWhether :: (a -> Bool) -> [a] -> ([a], [a])
splitWhether f target = (filter f target, filter (not . f) target)

splitSundayOrNot :: [KensinData] -> ([KensinData], [KensinData])
splitSundayOrNot dat = (`runReader` config) $ do
  sun <- sunday <$> ask
  return $ splitWhether ((==sun) . day) dat

genSortKey :: KParse (Day, Integer, Integer) -> Maybe String
genSortKey (Left _) = Nothing
genSortKey (Right (date, hour, minute)) =
  return $ show date ++ TP.printf "-%02d-%02d" hour minute

extractElement :: [String] -> CfgReader [String]
extractElement line = do
  let csvAry = listArray (0, length line) line
  map ((csvAry !) . fst) . extract <$> ask

toPayParse :: Parser String
toPayParse = many $ oneOf "1234567890・"

toPay :: String -> KParse Day -> KParse Option
toPay _ (Left x) = Left x
toPay str _ = 
  -- 最初に不適格な文字列("1234567890・"以外の文字で構成されている)を排除。
  case parse toPayParse "" str of
    Left s  -> Left s
    Right s -> case split '・' s of
                 [""] -> Left makeMessage
                 s'   -> Right s'
  where makeMessage =
          newErrorMessage (Expect "numStr combinated with a dot") (newPos "Base.hs" 164 0)

-- ["2","8","14"]などのオプション番号のリストから自己負担代を計算。
-- 仮引数paymentは上記の例でいうと、["2","8","14"]などのリスト。
-- fはfstかsndのどちらか。
makeAmountCore :: ((Integer, Integer) -> Integer) -> Option -> CfgReader Integer
makeAmountCore f payment = do
  ary <- vArray <$> ask
  -- [String]から[Int]への変換
  -- paymentが["2", "3", ""]の場合、
  -- map readEither payment → [Right 2, Right 3, Left ""]なので、
  -- rights $ map readEither payment → [2, 3]
  let paymentInt = rights $ map readEither payment
  return $ sum $ map (f. (ary !)) paymentInt

makeAmountOver40, makeAmountUnder40 :: Option -> Integer
makeAmountOver40  = (`runReader` config) . makeAmountCore snd
makeAmountUnder40 = (`runReader` config) . makeAmountCore fst

makeAmount :: Status -> Integer -> Option -> Integer
makeAmount st old' payment
  | st == Already = (+) 10000 $ makeAmountUnder40 payment
  | old' >= 40    = makeAmountOver40 payment
  | otherwise     = makeAmountUnder40 payment

hasAmount :: KensinData -> Bool
hasAmount kd = either (const False) (>0) $ amount kd

toTime :: KensinData -> String
toTime kd = case key kd of
  Right (_, h, m) -> TP.printf "%02d:%02d" h m
  Left _          -> ""
