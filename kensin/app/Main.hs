module Main where

import Util                             (makeMap, readUTF8File, runRubyString)
import Util.Strdt                       (howOld, nendoEnd, strdt)
import Util.StrEnum                     (split)
import KensinConfig
import Data.Time                        (Day)
import Data.Maybe                       (fromJust, isJust, mapMaybe)
import Data.Either                      (rights)
import Data.Monoid
import Data.Array
import Text.Read
import Control.Monad.Reader
import qualified Data.Map               as M
import qualified Data.Foldable          as F
import qualified Text.Printf            as TP
import qualified System.IO              as I

----------------------------------------------------------------------------------------------------
data Gender = Male | Female deriving (Show, Eq)
data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data KensinData = KensinData { day     :: Maybe Day
                             , sortKey :: Maybe String
                             , name    :: String
                             , gender  :: Gender
                             , old     :: Maybe Integer
                             , number  :: Maybe String
                             , kind    :: Kind
                             , stat    :: Status
                             , kday    :: String
                             , amount  :: Maybe Integer
                             , key     :: Maybe (Day, Integer, Integer)
                             , pay     :: Maybe [String]
                             , nonPay  :: Maybe [String] } deriving (Show, Eq)

instance Ord KensinData where
  compare (KensinData _ x _ _ _ _ _ _ _ _ _ _ _) (KensinData _ y _ _ _ _ _ _ _ _ _ _ _)
    | x > y = GT
    | x == y = EQ
    | otherwise = LT
----------------------------------------------------------------------------------------------------
type KensinBool = KensinData -> Bool

takeAny :: F.Foldable t => (a -> Any) -> t a -> Bool
takeAny f list =
  getAny $ F.foldMap f list

keyContains :: (KensinData -> Maybe [String]) -> [String] -> KensinBool
keyContains f opList kd =
  takeAny (Any . bool) opList
  where bool n' = case f kd of
          Just l -> n' `elem` l
          Nothing -> False

nonPayContains :: [String] -> KensinBool
nonPayContains = keyContains nonPay

payContains :: [String] -> KensinBool
payContains = keyContains pay

ladiesP :: [String] -> [String] -> KensinBool
ladiesP npList pList kd =
  takeAny (\ (f, key') -> Any $ f key' kd) [(nonPayContains, npList),
                                            (payContains, pList)]

ladies1P :: KensinBool
-- ladies1P = ladiesP ["4", "5", "6"] ["8", "9", "10"]
ladies1P = ladiesP ["5", "6", "7", "8", "9"] ["8", "9", "10"]

ladies2P :: KensinBool
-- ladies2P = ladiesP ["4", "5"] ["8", "9"]
ladies2P = ladiesP ["5", "6", "8", "9"] ["8", "9"]

jinpaiP :: KensinBool
jinpaiP = payContains ["13", "14"]

cameraP :: KensinBool
cameraP = payContains ["11"]

countIf :: (a -> Bool) -> [a] -> Int
countIf f = length . filter f

tokP :: KensinBool
tokP kd = fromJust $ (\o -> o>=40 && o <75) <$> old kd

numberCount :: [KensinData] -> [Int]
numberCount kds =
  map (`countIf` kds) [ladies1P, ladies2P, jinpaiP, cameraP] 
----------------------------------------------------------------------------------------------------
genSortKey :: (Day, Integer, Integer) -> String
genSortKey (date, hour, minute) =
  show date ++ TP.printf "-%02d-%02d" hour minute

-- [ n                     -- (0) 名前
--   , _                   -- (1) フリガナ1
--   , _                   -- (2) フリガナ2
--   , _                   -- (3) フリガナ3
--   , g                   -- (4) 性別
--   , birth               -- (5) 誕生日
--   , _                   -- (6) 年齢
--   , _                   -- (7) 保険証記号
--   , num                 -- (8) 保険証番号
--   , _                   -- (9) 本人/家族
--   , _                   -- (10) 保険証記号番号
--   , k                   -- (11) 本/家
--   , _                   -- (12) 住所
--   , _                   -- (13) 郵便番号
--   , _                   -- (14) 電話
--   , _                   -- (15) 組合員番号
--   , _                   -- (16) ？
--   , _                   -- (17) 世帯番号
--   , st                  -- (18) 受診フラグ
--   , day'                -- (19) 受付日時
--   , kday'               -- (20) 受診日時
--   , nop                 -- (21) 無料オプション
--   , op                  -- (22) 有料オプション
--   ]

extractElement :: [String] -> Reader Config [String]
extractElement line = do
  let csvAry = listArray (0, length line) line
  map ((csvAry !) . fst) . extract <$> ask


toPay :: Maybe Day -> String -> Maybe [String]
toPay Nothing _ = Nothing
toPay _ str =
  case split '・' str of
  [""] -> Nothing
  s' -> Just s'

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

type KensinPrice = (String, Integer, Integer)

-- ["2","8","14"]などのオプション番号のリストから自己負担代を計算。
-- 仮引数paymentは上記の例でいうと、["2","8","14"]などのリスト。
-- fはfstかsndのどちらか。
makeAmountCore :: ((Integer, Integer) -> Integer) -> [String] -> Reader Config Integer
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

lineToData :: [String] -> Reader Config KensinData
lineToData line = do
  nendo' <- year <$> ask
  let old' = flip howOld (nendoEnd nendo') <$> strdt birth
  return KensinData { day       = d
                    , kday      = kday'
                    , Main.name = n
                    , gender    = g'
                    , old       = old'
                    , kind      = kind'
                    , number    = number'
                    , stat      = stat'
                    , key       = key'
                    , amount    = makeAmount stat' <$> old' <*> pay'
                    , nonPay    = toPay d nop
                    , pay       = pay'
                    , sortKey   = genSortKey <$> key' }
  where [n, g, birth, num, k, st, day', kday', nop, op] = extractElement line `runReader` config
        key'       = toKey day'
        d          = fst3 <$> key'
        number'    = case num of ""   -> Nothing; s -> Just s
        g'         = case g   of "男" -> Male ;   _ -> Female
        stat'      = case st  of "1"  -> Already; _ -> Yet
        kind'      = case k   of "本" -> H;       _ -> K
        pay'       = toPay d op
        
toKey :: String -> Maybe (Day, Integer, Integer)
toKey str = 
  case split ' ' str of
  [date', time', _] -> Just (d, hour, minute)
    where [hour, minute, _] = map read $ split ':' time'
          d = fromJust $ strdt date'
  _ -> Nothing

toCsvData :: [String] -> [KensinData]
toCsvData = filter (isJust . key) . map ((`runReader` config) . lineToData . split ',')

makeKensinMap :: [KensinData] -> M.Map (Maybe String) [KensinData]
makeKensinMap = makeMap sortKey id

translateJusin :: [KensinData] -> [(Maybe String, [Int])]
translateJusin =
  map count' . M.toList . makeKensinMap
  where count' (k, v) = (k, length v:numberCount v)

translateAmount :: [KensinData] -> [(String, Maybe Day, Maybe Integer)]
translateAmount =
  map (\kd -> (Main.name kd, day kd, amount kd)) . filterAmount
  where filterAmount = filter (isJust . amount)

amountShow :: (String, Maybe Day, Maybe Integer) -> String
amountShow (_, Nothing, _) = ""
amountShow (_, _, Nothing) = ""
amountShow (name, Just d, Just a) =
  name ++ "," ++ show d ++ "," ++ show a

counter :: (a -> Bool) -> (KensinData -> a) -> [KensinData] -> Int
counter key field kds = length $ filter (key . field) kds

hkCount :: [KensinData] -> (Int, Int)
hkCount kd = (count' (==H), count' (==K))
  where count' f = counter f kind kd

-- 特定健診該当年齢とそれ以外をカウントする。
tokCount :: [KensinData] -> (Int, Int)
tokCount kd = (count' tokP, count' (not . tokP))
  where list'    = mapMaybe old kd
        tokP y'  = (y'>=40) && (y'<75)
        count' f = length $ filter f list'

baseInfo :: [KensinData] -> String
baseInfo kds =
  TP.printf "組合員本人: %d人、 家族: %d人\n特定健診: %d人、 以外: %d人" h' k' tok notTok
  where (h', k') = hkCount kds
        (tok, notTok) = tokCount kds

csvData :: ReaderT Config IO [KensinData]
csvData = do
  file' <- file <$> ask
  contents <- liftIO $ readUTF8File file'
  return $ toCsvData $ lines contents

csvRubyData :: ReaderT Config IO [KensinData]
csvRubyData = do
  file' <- excelFile <$> ask
  prog  <- rubyProg <$> ask
  contents <- liftIO $ runRubyString [prog, file']
  return $ toCsvData contents

main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  csv  <- csvRubyData `runReaderT` config
  let showS = mapM_ print
  showS $ translateJusin csv
  mapM_ (putStrLn . amountShow) $ translateAmount csv
