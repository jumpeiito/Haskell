module Main where

import Util                             (makeMap, readUTF8File, runRubyString)
import Util.StrEnum                     (split)
import Kensin.Base
import Kensin.Meibo
import Kensin.Config                    (Config (..), config)
import Kensin.Receipt                   (receiptSunday, receiptWeekday, toReceipt)
import Data.List                        (sortBy, intercalate)
import Data.Time                        (Day)
import Data.Monoid
import Control.Monad.Reader
import qualified Data.Map               as M
import qualified Text.Printf            as TP
import qualified System.IO              as I
import qualified Options.Applicative    as O
----------------------------------------------------------------------------------------------------
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
ladiesP npList pList kd = nonPayContains npList kd || payContains pList kd

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

translateAmount :: [KensinData] -> [(String, Day, KParse Integer)]
translateAmount =
  map (\kd -> (Kensin.Base.name kd, day kd, amount kd)) . filterAmount
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
  where list'    = map old kd
        tokP y'  = (y'>=40) && (y'<75)
        count' f = length $ filter f list'

baseInfo :: [KensinData] -> String
baseInfo kds =
  TP.printf "組合員本人: %d人、 家族: %d人\n特定健診: %d人、 以外: %d人" h' k' tok notTok
  where (h', k') = hkCount kds
        (tok, notTok) = tokCount kds
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
--main----------------------------------------------------------------------------------------------
main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  csv  <- csvRubyData `runReaderT` config
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo

  let jusin = mapM_ (putStrLn . jusinShowLine) . translateJusin

  case (count' opt, receipt' opt, meibo' opt) of
    (True, False, False) -> jusin csv
    (False, True, False) -> do
      putStrLn $ toReceipt $ receiptSunday csv
      putStrLn $ toReceipt $ receiptWeekday csv
    (False, False, True) -> meiboOutput csv
    (False, False, False) -> jusin csv
    (_, _, _) -> jusin csv
--Command Line Option-------------------------------------------------------------------------------
data Options = Options { count'    :: Bool
                       , receipt'  :: Bool
                       , meibo'    :: Bool
                       } deriving (Show)

countP :: O.Parser Bool
countP = O.switch $ O.short 'c' <> O.long "count" <> O.help "Count day by day."

receiptP :: O.Parser Bool
receiptP = O.switch $ O.short 'r' <> O.long "receipt" <> O.help "Output receipts."

meiboP :: O.Parser Bool
meiboP = O.switch $ O.short 'm' <> O.long "meibo" <> O.help "Output meibo."

optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> countP
           <*> receiptP
           <*> meiboP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "Program for kensin."
    , O.header "kensin.exe -- program for kensin."
    , O.footer ""
    , O.progDesc ""
    ]
