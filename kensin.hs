{-# LANGUAGE DeriveDataTypeable #-}

import Util
import Strdt
import Data.Time
import Data.List
import Data.Maybe
import Data.Monoid
-- import Control.Monad
import Control.Applicative
import qualified Data.Map               as M
-- import System.Console.CmdArgs as Arg
import qualified Data.Foldable          as F
import qualified Text.Printf            as TP
import qualified System.IO              as I

data Config = Con { file       :: String
                  , keyColNum  :: Int
                  , nendo      :: Integer
                  , valueAlist :: [(String, Integer, Integer)] }

config = Con { file       = "f:/Haskell/.kensin"
             , keyColNum  = 12
             , nendo      = 2016
             , valueAlist =
               [("1", 6000,3000), ("2", 2500,2500), ("3", 8500,5500),
                ("4", 1500,1500), ("5", 1500, 500), ("6", 1500, 500),
                ("7", 1500, 500), ("8", 5000,0), ("9", 5000,0),
                ("10",3000,2000), ("11",8000,4000), ("12",1000,1000),
                ("13",3500,3500), ("14",3500,3500)]}

file      :: String
keyColNum :: Int
nendo     :: Integer
file      = "f:/Haskell/.kensin"
keyColNum = 12
nendo     = 2016
----------definition--------------------------------------------------------------------------------
valueAlist :: [(String, Integer, Integer)]
valueAlist = [("1", 6000,3000), ("2", 2500,2500), ("3", 8500,5500),
              ("4", 1500,1500), ("5", 1500, 500), ("6", 1500, 500),
              ("7", 1500, 500), ("8", 5000,0), ("9", 5000,0),
              ("10",3000,2000), ("11",8000,4000), ("12",1000,1000),
              ("13",3500,3500), ("14",3500,3500)]

-- data Option = Option
--               { optKensin :: Maybe String,
--                 optAmount :: Maybe String } deriving (Show, Data, Typeable)

-- option :: Option
-- option = Option { optKensin = Nothing &= Arg.name "kensin" &= explicit,
--                   optAmount = Nothing &= Arg.name "amount" &= explicit}
----------------------------------------------------------------------------------------------------
data Gender = Male | Female deriving (Show, Eq)
data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data KensinData = KensinData { day     :: Maybe Day,
                               sortKey :: Maybe String,
                               name    :: String, 
                               gender  :: Gender,
                               old     :: Maybe Integer, 
                               number  :: Maybe String,
                               kind    :: Kind,
                               stat    :: Status,
                               kday    :: String, 
                               amount  :: Maybe Integer,
                               key     :: Maybe (Day, Integer, Integer),
                               pay     :: Maybe [String],
                               nonPay  :: Maybe [String] } deriving (Show, Eq)

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

extractAlist :: [(Integer, String)]
extractAlist = [(0, "氏名"), (2, "性別"), (3, "生年月日"),
                (5, "保険証番号"),  (6, "区分"), (7, "補助"),
                (10, "日時"), (11, "申込日時"), (12, "無料オプション"),
                (13, "有料オプション")]

extractElement :: [String] -> [String]
extractElement line =
  map snd $ filter numberExtract cols
  where cols = zip [0..] line
        tal  = map fst extractAlist
        numberExtract (n, _) = n `elem` tal

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

makeAmountCore :: (KensinPrice -> Integer) -> [String] -> Integer
makeAmountCore f payment =
  foldl (+) 0 $ mapMaybe id $ map coref payment
  where coref key' = f <$> lookup3 key' valueAlist

makeAmountOver40 :: [String] -> Integer
makeAmountOver40 = makeAmountCore thd3

makeAmountUnder40 :: [String] -> Integer
makeAmountUnder40 = makeAmountCore snd3

makeAmount :: Status -> Integer -> [String] -> Integer
makeAmount st old' payment
  | st == Already = (+) 10000 $ makeAmountUnder40 payment
  | old' >= 40    = makeAmountOver40 payment
  | otherwise     = makeAmountUnder40 payment

lineToData :: [String] -> KensinData
lineToData line =
  KensinData { day       = d,
               kday      = kday',
               Main.name = n,
               gender    = g',
               old       = old',
               kind      = kind',
               number    = number',
               stat      = stat',
               key       = key',
               amount    = makeAmount stat' <$> old' <*> pay',
               nonPay    = toPay d nop,
               pay       = pay',
               sortKey   = genSortKey <$> key' }
  where [n, g, birth, num, k, st, day', kday', nop, op] = extractElement line
        key'       = toKey day'
        d          = fst3 <$> key'
        number'    = case num of ""   -> Nothing; s -> Just s
        g'         = case g   of "男" -> Male ;   _ -> Female
        stat'      = case st  of "1"  -> Already; _ -> Yet
        kind'      = case k   of "本" -> H;       _ -> K
        old'       = flip howOld (nendoEnd nendo) <$> (strdt birth :: Maybe Day)
        pay'       = toPay d op
        
toKey :: String -> Maybe (Day, Integer, Integer)
toKey str = 
  case split ' ' str of
  [date', time'] -> Just (d, hour, minute)
    where [hour, minute] = map stoi $ split ':' time'
          stoi n = read n :: Integer
          d = fromJust $ (strdt date' :: Maybe Day)
  _ -> Nothing

toCsvData :: String -> [KensinData]
toCsvData = filter (isJust . key) . map (lineToData . split ',') . lines

makeMap :: [KensinData] -> M.Map (Maybe String) [KensinData]
makeMap = foldl coref M.empty
  where coref map' kd = 
          let key' = sortKey kd in
          case M.lookup key' map' of
          Just xl -> M.insert key' (kd:xl) map'
          Nothing -> M.insert key' [kd] map'

translateJusin :: [KensinData] -> [(Maybe String, [Int])]
translateJusin =
  map count' . M.toList . makeMap
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
  contents <- liftIO $ readUTF8File <$> file <$> ask
  return $ toCsvData contents

main :: IO ()
main = do
  -- opt <- cmdArgs option
  -- print opt
  I.hSetEncoding I.stdout I.utf8
  cont <- sort <$> toCsvData <$> readUTF8File file
  -- let showS = mapM_ (putStrLn . show)
  -- showS $ translateJusin cont
  mapM_ (putStrLn . amountShow) $ translateAmount cont
