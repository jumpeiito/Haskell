module Main where

import Util                             (readUTF8File, runRubyString)
import Util.StrEnum                     (split)
import Kensin.Base
import Kensin.Meibo
import Kensin.Config                    (Config (..), config, Gender (..))
import Kensin.Receipt                   (receiptSunday, receiptWeekday, toReceipt)
import Kensin.Count                     (jusinShowLine, translateJusin)
import Data.Monoid
import Control.Monad.Reader
import qualified Text.Printf            as TP
import qualified System.IO              as I
import qualified Options.Applicative    as O
--basic info----------------------------------------------------------------------------------------
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
--fundamental---------------------------------------------------------------------------------------
toCsvData :: [String] -> [KensinData]
toCsvData = filter (isRight . key) .
            map ((`runReader` config) .
                 lineToData .
                 split ',')
  where isRight (Right _) = True
        isRight (Left _)  = False

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
    (False, False, True) -> meiboOutput csv `runReaderT` config
    (False, False, False) -> jusin csv
    (_, _, _) -> jusin csv
--Command Line Option-------------------------------------------------------------------------------
data Options = Options { count'    :: Bool
                       , receipt'  :: Bool
                       , meibo'    :: Bool
                       , base'     :: Bool
                       } deriving (Show)

countP :: O.Parser Bool
countP = O.switch $ O.short 'c' <> O.long "count" <> O.help "Count day by day."

receiptP :: O.Parser Bool
receiptP = O.switch $ O.short 'r' <> O.long "receipt" <> O.help "Output receipts."

meiboP :: O.Parser Bool
meiboP = O.switch $ O.short 'm' <> O.long "meibo" <> O.help "Output meibo."

baseP :: O.Parser Bool
baseP = O.switch $ O.short 'b' <> O.long "base" <> O.help "Output basic info."

optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> countP
           <*> receiptP
           <*> meiboP
           <*> baseP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "Program for kensin."
    , O.header "kensin.exe -- program for kensin."
    , O.footer ""
    , O.progDesc ""
    ]
