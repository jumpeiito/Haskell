module Main where

import Util                             (makeMap, readUTF8File, runRubyString)
import Util.Strdt                       (howOld, nendoEnd, strdt)
import Util.StrEnum                     (split)
import KensinConfig
import Data.List                        (intercalate)
import Data.Time                        (Day, fromGregorian)
import Data.Maybe                       (fromJust, isJust, mapMaybe, fromMaybe)
import Data.Either                      (rights)
import Data.Array                       ((!), listArray)
import Text.Read                        (readEither)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad.Reader
import qualified Data.Map               as M
import qualified Data.Foldable          as F
import qualified Text.Printf            as TP
import qualified System.IO              as I

----------------------------------------------------------------------------------------------------
data Gender = Male | Female deriving (Show, Eq)
data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data KensinData = KensinData { day     :: KParse Day
                             , sortKey :: Maybe String
                             , name    :: String
                             , gender  :: Gender
                             , old     :: Integer
                             , number  :: Maybe String
                             , kind    :: Kind
                             , stat    :: Status
                             , kday    :: String
                             , amount  :: KParse Integer
                             , key     :: KParse (Day, Integer, Integer)
                             , pay     :: KParse [String]
                             , nonPay  :: KParse [String] } deriving (Show, Eq)

instance Ord KensinData where
  compare (KensinData _ x _ _ _ _ _ _ _ _ _ _ _) (KensinData _ y _ _ _ _ _ _ _ _ _ _ _)
    | x > y = GT
    | x == y = EQ
    | otherwise = LT
----------------------------------------------------------------------------------------------------
type KensinBool  = KensinData -> Bool
type CfgReader   = Reader Config
type CfgReaderT  = ReaderT Config IO
type KensinPrice = (String, Integer, Integer)
type KParse      = Either ParseError
----------------------------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

keyContains :: (KensinData -> KParse [String]) -> [String] -> KensinBool
keyContains f opList kd = any bool opList
  where bool n = case elem n <$> f kd of
                   Right x -> x
                   _ -> False

nonPayContains :: [String] -> KensinBool
nonPayContains = keyContains nonPay

payContains :: [String] -> KensinBool
payContains = keyContains pay

ladiesP :: [String] -> [String] -> KensinBool
ladiesP npList pList kd = any id [nonPayContains npList kd , payContains pList kd]

ladies1P, ladies2P, jinpaiP, cameraP, tokP :: KensinBool
ladies1P = ladiesP ["5", "6", "7", "8", "9"] ["8", "9", "10"] -- 乳がん・子宮がん
ladies2P = ladiesP ["5", "6", "8", "9"] ["8", "9"]            -- 乳がんのみ
jinpaiP  = payContains ["13", "14"]
cameraP  = payContains ["11"]
tokP kd  = old kd>=40 && old kd<75

countIf :: (a -> Bool) -> [a] -> Int
countIf f = length . filter f

numberCount :: [KensinData] -> [(String, Int)]
numberCount kds =
  map (\(str, f) -> (str, countIf f kds))
                [ ("全女性検診", ladies1P)
                , ("乳がんのみ", ladies2P)
                , ("アスベスト", jinpaiP)
                , ("胃カメラ", cameraP)]
----------------------------------------------------------------------------------------------------
genSortKey :: KParse (Day, Integer, Integer) -> Maybe String
genSortKey (Left _) = Nothing
genSortKey (Right (date, hour, minute)) =
  return $ show date ++ TP.printf "-%02d-%02d" hour minute

extractElement :: [String] -> CfgReader [String]
extractElement line = do
  let csvAry = listArray (0, length line) line
  map ((csvAry !) . fst) . extract <$> ask

toPay :: String -> KParse Day -> KParse [String]
toPay _ (Left x) = Left x
toPay str _ = case split '・' str of
  [""] -> Left makeMessage
  s'   -> Right s'
  where makeMessage =
          newErrorMessage (Expect "numStr combinated with '・'") (newPos "Main.hs" 99 0)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- ["2","8","14"]などのオプション番号のリストから自己負担代を計算。
-- 仮引数paymentは上記の例でいうと、["2","8","14"]などのリスト。
-- fはfstかsndのどちらか。
makeAmountCore :: ((Integer, Integer) -> Integer) -> [String] -> CfgReader Integer
makeAmountCore f payment = do
  ary <- vArray <$> ask
  -- [String]から[Int]への変換
  let paymentInt = rights $ map readEither payment
  return $ sum $ map (f. (ary !)) paymentInt

makeAmountOver40, makeAmountUnder40 :: [String] -> Integer
makeAmountOver40  = (`runReader` config) . makeAmountCore snd
makeAmountUnder40 = (`runReader` config) . makeAmountCore fst

makeAmount :: Status -> Integer -> [String] -> Integer
makeAmount st old' payment
  | st == Already = (+) 10000 $ makeAmountUnder40 payment
  | old' >= 40    = makeAmountOver40 payment
  | otherwise     = makeAmountUnder40 payment

(==>) :: (String, String) -> (a, a) -> a
(sym, target) ==> (yes, no)
  | sym == target = yes
  | otherwise     = no

lineToData :: [String] -> CfgReader KensinData
lineToData line = do
  nendo' <- year <$> ask
  let old' = fromMaybe 0 $ (`howOld` (nendoEnd nendo')) <$> strdt birth
  return KensinData { day       = d
                    , kday      = kday'
                    , Main.name = n
                    , gender    = g'
                    , old       = old'
                    , kind      = kind'
                    , number    = number'
                    , stat      = stat'
                    , key       = key'
                    , amount    = makeAmount stat' old' <$> pay'
                    , nonPay    = toPay nop d
                    , pay       = pay'
                    , sortKey   = genSortKey key' }
  where [n, g, birth, num, k, st, day', kday', nop, op] = extractElement line `runReader` config
        key'       = toKey day'
        d          = fst3 <$> key'
        number'    = case num of ""   -> Nothing; s -> Just s
        g'         = (g,  "男") ==> (Male, Female)
        stat'      = (st, "1")  ==> (Already, Yet)
        kind'      = (k,  "本") ==> (H, K)
        pay'       = toPay op d
        
toKeyParse :: Parser (Day, Integer, Integer)
toKeyParse = do
  year'   <- read <$> count 4 digit <* char '-'
  month'  <- read <$> count 2 digit <* char '-'
  day'    <- read <$> count 2 digit <* char ' '
  hour'   <- read <$> count 2 digit <* char ':'
  minute' <- read <$> count 2 digit <* many anyChar
  return $ (fromGregorian year' month' day', hour', minute')

toKey :: String -> KParse (Day, Integer, Integer)
toKey str = parse toKeyParse "" str

toCsvData :: [String] -> [KensinData]
toCsvData = filter (isRight . key) .
            map ((`runReader` config) .
                 lineToData .
                 split ',')

makeKensinMap :: [KensinData] -> M.Map (Maybe String) [KensinData]
makeKensinMap = makeMap sortKey id

translateJusin :: [KensinData] -> [(Maybe String, [(String, Int)])]
translateJusin =
  map count' . M.toList . makeKensinMap
  where count' (k, v) = (k, ("全受診者", length v):numberCount v)

translateAmount :: [KensinData] -> [(String, KParse Day, KParse Integer)]
translateAmount =
  map (\kd -> (Main.name kd, day kd, amount kd)) . filterAmount
  where filterAmount = filter (isRight . amount)

jusinShowPair :: (String, Int) -> String
jusinShowPair (title, len)
  | len == 0  = "------------"
  | otherwise = TP.printf "%s: %d" title len

jusinShowLine :: (Maybe String, [(String, Int)]) -> String
jusinShowLine (Nothing, _) = ""
jusinShowLine (Just date, pairs) =
  TP.printf "%s :: %s\t:: %s" date' ps date'
  where ps = intercalate "\t" $ map jusinShowPair pairs
        date' = drop 5 date

jusinShow :: [(Maybe String, [(String, Int)])] -> [String]
jusinShow = map jusinShowLine

amountShow :: (String, KParse Day, KParse Integer) -> String
amountShow (_, Left _, _) = ""
amountShow (_, _, Left _) = ""
amountShow (name, Right d, Right a) =
  name ++ "," ++ show d ++ "," ++ show a

counter :: (a -> Bool) -> (KensinData -> a) -> [KensinData] -> Int
counter key field kds = length $ filter (key . field) kds

hkCount :: [KensinData] -> (Int, Int)
hkCount kd = (count' (==H), count' (==K))
  where count' f = counter f kind kd

-- 特定健診該当年齢とそれ以外をカウントする。
tokCount :: [KensinData] -> (Int, Int)
tokCount kd = (count' tokP, count' (not . tokP))
  where list'    = map old kd -- mapMaybe old kd
        tokP y'  = (y'>=40) && (y'<75)
        count' f = length $ filter f list'

baseInfo :: [KensinData] -> String
baseInfo kds =
  TP.printf "組合員本人: %d人、 家族: %d人\n特定健診: %d人、 以外: %d人" h' k' tok notTok
  where (h', k') = hkCount kds
        (tok, notTok) = tokCount kds

csvData :: CfgReaderT [KensinData]
csvData = do
  file' <- file <$> ask
  contents <- liftIO $ readUTF8File file'
  return $ toCsvData $ lines contents

csvRubyData :: CfgReaderT [KensinData]
csvRubyData = do
  file' <- excelFile <$> ask
  prog  <- rubyProg <$> ask
  contents <- liftIO $ runRubyString [prog, file']
  return $ toCsvData contents

main :: IO ()
main = do
  -- sjis <- I.mkTextEncoding "CP932"
  -- -- SJISで出力 (-s)
  -- when (sjis' opt) $ I.hSetEncoding I.stdout sjis
  I.hSetEncoding I.stdout I.utf8
  csv  <- csvRubyData `runReaderT` config
  mapM_ (putStrLn . jusinShowLine) $ translateJusin csv
  mapM_ (putStrLn . amountShow) $ translateAmount csv
