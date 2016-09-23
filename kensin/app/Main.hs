{-# LANGUAGE DeriveDataTypeable #-}

import Util
import Util.Strdt
import Util.StrEnum                     (split)
import Data.Time
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map               as M
-- import System.Console.CmdArgs as Arg
import qualified Data.Foldable          as F
import qualified Text.Printf            as TP
import qualified System.IO              as I

data Config = Con { file       :: FilePath
                  , excelFile  :: FilePath
                  , rubyProg   :: FilePath
                  , keyColNum  :: Int
                  , year       :: Integer
                  , valueAlist :: [(String, Integer, Integer)] }

config = Con { file       = "f:/Haskell/.kensin"
             , excelFile  = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
             , rubyProg   = "f:/Haskell/kensin/kensin.rb"
             , keyColNum  = 12
             , year       = 2016
             , valueAlist =
               [ ("1", 6000, 3000) -- 胃バリウム
               , ("2", 2500, 2500) -- 腹部エコー
               , ("3", 8500, 5500) -- 胃バリウム + 腹部エコー
               , ("4", 1500, 1500) -- 肝炎ウィルス
               , ("5", 1500,  500) -- ペプシノーゲン
               , ("6", 1500,  500) -- ピロリ菌
               , ("7", 1500,  500) -- 前立腺がん
               , ("8", 5000,    0) -- 乳がん・マンモ
               , ("9", 5000,    0) -- 乳がん・乳腺エコー
               , ("10",3000, 2000) -- 子宮がん
               , ("11",8000, 4000) -- 胃カメラ
               , ("12",1000, 1000) -- 骨密度
               , ("13",3500, 3500) -- アスベスト
               , ("14",3500, 3500) -- じん肺
               ]}

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
  takeAny (\n -> Any $ bool n) opList
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
  map (flip countIf kds) [ladies1P, ladies2P, jinpaiP, cameraP] 
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

extractAlist :: [(Integer, String)]
extractAlist = [ (0, "氏名")
               , (4, "性別")
               , (5, "生年月日")
               , (10, "保険証番号")
               , (9, "区分")
               , (18, "補助")
               , (19, "日時")
               , (20, "申込日時")
               , (21, "無料オプション")
               , (22, "有料オプション")]

extractElement :: [String] -> [String]
extractElement line =
  map snd $ filter numberExtract csvLine
  where csvLine  = zip [0..] line
        alistNum = map fst extractAlist
        numberExtract (n, _) = n `elem` alistNum

toPay :: Maybe Day -> String -> Maybe [String]
toPay Nothing _ = Nothing
toPay _ str =
  case split '・' str of
  [""] -> Nothing
  s' -> Just s'

lookup3 :: (Eq a) => a -> [(a,b,c)] -> Maybe (a,b,c)
lookup3 _ [] = Nothing
lookup3 key' (v@(a',_,_):as)
  | key' == a' = Just v
  | otherwise  = lookup3 key' as

fst3 :: (a, b, c) -> a
snd3 :: (a, b, c) -> b
thd3 :: (a, b, c) -> c
fst3 (a, _, _) = a
snd3 (_, a, _) = a
thd3 (_, _, a) = a

type KensinPrice = (String, Integer, Integer)

makeAmountCore :: (KensinPrice -> Integer) -> [String] -> Reader Config Integer
makeAmountCore f payment = do
  alist <- valueAlist <$> ask
  let seek payStr = f <$> lookup3 payStr alist
  return $ foldl (+) 0 $ mapMaybe id $ map seek payment

makeAmountOver40 :: [String] -> Integer
makeAmountOver40 = (`runReader` config) . makeAmountCore thd3

makeAmountUnder40 :: [String] -> Integer
makeAmountUnder40 = (`runReader` config) . makeAmountCore snd3

makeAmount :: Status -> Integer -> [String] -> Integer
makeAmount st old' payment
  | st == Already = (+) 10000 $ makeAmountUnder40 payment
  | old' >= 40    = makeAmountOver40 payment
  | otherwise     = makeAmountUnder40 payment

lineToData :: [String] -> Reader Config KensinData
lineToData line = do
  nendo' <- year <$> ask
  let old' = flip howOld (nendoEnd nendo') <$> (strdt birth :: Maybe Day)
  return $ KensinData { day       = d
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
  where [n, g, birth, num, k, st, day', kday', nop, op] = extractElement line
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
          d = fromJust $ (strdt date')
  _ -> Nothing

toCsvData :: [String] -> [KensinData]
toCsvData = filter (isJust . key) . map ((`runReader` config) . lineToData . split ',')

makeKensinMap = makeMap sortKey id

translateJusin :: [KensinData] -> [(Maybe String, [Int])]
translateJusin =
  map count' . M.toList . makeKensinMap
  where count' (k, v) = (k, (length v):(numberCount v))

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
  let showS = mapM_ (putStrLn . show)
  showS $ translateJusin csv
