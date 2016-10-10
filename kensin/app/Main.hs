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
import Test.Hspec
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
csvRubyData :: CfgReaderT [KensinData]
csvRubyData = do
  cfg      <- liftIO config
  file'    <- excelFile <$> ask
  prog     <- rubyProg <$> ask
  contents <- liftIO $ runRubyString [prog, file']
  return $ toCsvData contents `runReader` cfg
--main----------------------------------------------------------------------------------------------
getEncoding :: CfgReader (IO I.TextEncoding)
getEncoding = encoding <$> ask

main :: IO ()
main = do
  cfg <- config

  encode <- getEncoding `runReader` cfg
  I.hSetEncoding I.stdout encode

  csv  <- csvRubyData `runReaderT` cfg
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo

  let jusin kds = mapM_ (putStrLn . jusinShowLine) $ translateJusin kds `runReader` cfg

  case (count' opt, receipt' opt, meibo' opt) of
    (True, False, False) -> jusin csv
    (False, True, False) -> do
      let s = receiptSunday csv `runReader` cfg
      let w = receiptWeekday csv `runReader` cfg
      putStrLn $ toReceipt s `runReader` cfg
      putStrLn $ toReceipt w `runReader` cfg
    (False, False, True) -> putStrLn (meiboOutput csv `runReader` cfg)
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
--test----------------------------------------------------------------------------------------------

