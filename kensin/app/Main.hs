module Main where

import Util                             (makeMap, readUTF8File, runRubyString, group, ketaNum)
import Util.Strdt                       (howOld, nendoEnd, strdt, dayStrWithSep)
import Util.StrEnum                     (split)
import KensinConfig                     (Config (..), config)
import Data.List                        (sortBy, intercalate)
import Data.Time                        (Day, fromGregorian)
import Data.Maybe                       (fromJust, isJust, mapMaybe, fromMaybe)
import Data.Either                      (rights)
import Data.Array                       ((!), listArray)
import Data.Monoid
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
import qualified Options.Applicative    as O
----------------------------------------------------------------------------------------------------
data Gender = Male | Female deriving (Show, Eq)
data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data Bunkai = Ishida
  | Hino
  | Ogurisu
  | Ichigonji
  | Sampoin
  | Tenzai deriving (Show, Ord, Eq, Enum)
data KensinData = KensinData { day     :: Day
                             , sortKey :: Maybe String
                             , name    :: String
                             , gender  :: Gender
                             , old     :: Integer
                             , bunkai  :: Bunkai
                             , number  :: Maybe String
                             , kind    :: Kind
                             , stat    :: Status
                             , kday    :: String
                             , amount  :: KParse Integer
                             , key     :: KParse (Day, Integer, Integer)
                             , pay     :: KParse [String]
                             , nonPay  :: KParse [String] } deriving (Show, Eq)

instance Ord KensinData where
  compare (KensinData _ x _ _ _ _ _ _ _ _ _ _ _ _) (KensinData _ y _ _ _ _ _ _ _ _ _ _ _ _)
    | x > y = GT
    | x == y = EQ
    | otherwise = LT
----------------------------------------------------------------------------------------------------
type KensinBool  = KensinData -> Bool
type CfgReader   = Reader Config
type CfgReaderT  = ReaderT Config IO
type KensinPrice = (String, Integer, Integer)
type KParse      = Either ParseError
type Option      = [String]
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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

keyContains :: (KensinData -> KParse Option) -> Option -> KensinBool
keyContains f opList kd = any bool opList
  where bool n = case elem n <$> f kd of
                   Right x -> x
                   _ -> False

nonPayContains :: Option -> KensinBool
nonPayContains = keyContains nonPay

payContains :: Option -> KensinBool
payContains = keyContains pay

ladiesP :: Option -> Option -> KensinBool
ladiesP npList pList kd = or [nonPayContains npList kd , payContains pList kd]

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

toPay :: String -> KParse Day -> KParse Option
toPay _ (Left x) = Left x
toPay str _ = case split '・' str of
  [""] -> Left makeMessage
  s'   -> Right s'
  where makeMessage =
          newErrorMessage (Expect "numStr combinated with a dot") (newPos "Main.hs" 99 0)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- ["2","8","14"]などのオプション番号のリストから自己負担代を計算。
-- 仮引数paymentは上記の例でいうと、["2","8","14"]などのリスト。
-- fはfstかsndのどちらか。
makeAmountCore :: ((Integer, Integer) -> Integer) -> Option -> CfgReader Integer
makeAmountCore f payment = do
  ary <- vArray <$> ask
  -- [String]から[Int]への変換
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
                    , Main.name = n
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
  where [bk, n, g, birth, num, k, st, day', kday', nop, op] = extractElement line `runReader` config
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

hasAmount :: KensinData -> Bool
hasAmount kd = case amount kd of
  Right x -> x > 0
  Left _  -> False

translateAmount :: [KensinData] -> [(String, Day, KParse Integer)]
translateAmount =
  map (\kd -> (Main.name kd, day kd, amount kd)) . filterAmount
  where filterAmount = filter hasAmount

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

amountShow :: (String, Day, KParse Integer) -> String
amountShow (_, _, Left _) = ""
amountShow (name, d, Right a) =
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
----------------------------------------------------------------------------------------------------
splitWhether :: (a -> Bool) -> [a] -> ([a], [a])
splitWhether f target = (filter f target, filter (not . f) target)

splitSundayOrNot :: [KensinData] -> ([KensinData], [KensinData])
splitSundayOrNot dat = (`runReader` config) $ do
  sun <- sunday <$> ask
  return $ splitWhether ((==sun) . day) dat

sortByDay, sortByBunkai :: [KensinData] -> [KensinData]
sortByDay kd    = sortBy (\x y -> compare (day x) (day y)) kd
sortByBunkai kd = sortBy (\x y -> compare (bunkai x) (bunkai y)) kd

makeReceiptData :: (([KensinData], [KensinData]) -> [KensinData]) -> -- fst or snd
                   ([KensinData] -> [KensinData]) ->                 -- sort Function
                   [KensinData] ->
                   [KensinData]
makeReceiptData f sortf = sortf . filter hasAmount . f . splitSundayOrNot

receiptSunday, receiptWeekday :: [KensinData] -> [KensinData]
receiptSunday  = makeReceiptData fst sortByBunkai
receiptWeekday = makeReceiptData snd sortByDay

kensinDataToReceipt :: KensinData -> String
kensinDataToReceipt kd = "\\writer" ++ arguments
  where arguments = concatMap enclose [ dayStrWithSep '/' $ day kd
                                      , bunkaiToStr $ bunkai kd
                                      , name kd
                                      , ketaNum $ show $ either (const 0) id $ amount kd]
        enclose str = "{" ++ str ++ "}"

toReceiptPage :: [KensinData] -> String
toReceiptPage kds = "\\begin{receiptPage}" ++
                    concatMap kensinDataToReceipt kds ++
                    "\\end{receiptPage}"

toReceipt :: [KensinData] -> String
toReceipt kds = concatMap toReceiptPage $ group 5 kds
----------------------------------------------------------------------------------------------------
csvData :: CfgReaderT [KensinData]
csvData = do
  file'    <- file <$> ask
  contents <- liftIO $ readUTF8File file'
  return $ toCsvData $ lines contents

csvRubyData :: CfgReaderT [KensinData]
csvRubyData = do
  file'    <- excelFile <$> ask
  prog     <- rubyProg <$> ask
  contents <- liftIO $ runRubyString [prog, file']
  return $ toCsvData contents

main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  csv  <- csvRubyData `runReaderT` config
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo
  case (count' opt, receipt' opt) of
    (True, _) -> mapM_ (putStrLn . jusinShowLine) $ translateJusin csv
    (_, True) -> do
      putStrLn $ toReceipt $ receiptSunday csv
      putStrLn $ toReceipt $ receiptWeekday csv
    (_, _)    -> mapM_ (putStrLn . jusinShowLine) $ translateJusin csv

data Options = Options { count'    :: Bool
                       , receipt'  :: Bool
                       } deriving (Show)

countP :: O.Parser Bool
countP = O.switch $ O.short 'c' <> O.long "count" <> O.help "Count day by day."

receiptP :: O.Parser Bool
receiptP = O.switch $ O.short 'r' <> O.long "receipt" <> O.help "Output receipts."

optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> countP
           <*> receiptP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "Program for kensin."
    , O.header "kensin.exe -- program for kensin."
    , O.footer ""
    , O.progDesc ""
    ]    
