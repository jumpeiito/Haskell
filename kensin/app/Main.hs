module Main where

import Util                             (readUTF8File, runRubyString, locEncoding, writeUTF8File)
import Util.StrEnum                     (split)
import Kensin.Base
import Kensin.Meibo
import Kensin.Config                    (Config (..), config, Gender (..), ShowDirector (..))
import Kensin.Receipt                   (receiptSunday, receiptWeekday, toReceipt)
import Kensin.Count                     (jusinShowLine, translateJusin, ladies1P, cameraP, jinpaiP)
import Data.Monoid
import Data.List                        (sort)
import Control.Monad.Reader
import System.Directory                 (getModificationTime, doesFileExist)
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

normalPrint :: [KensinData] -> CfgReaderT ()
normalPrint kds = do
  direct <- optionDirector <$> ask
  -- liftIO locEncoding
  mapM_ (liftIO . putStrLn . translateFree direct) kds

-- writeCSVfile :: CfgReaderT ()
-- writeCSVfile = do
--   output   <- outputCSV <$> ask
--   excelM   <- liftIO $ getModificationTime <$> excelFile <$> ask
--   outputP  <- liftIO $ doesFileExist output
--   outputM  <- if outputP then liftIO $ getModificationTime output else return False
--   file'    <- excelFile <$> ask
--   prog     <- rubyProg  <$> ask
--   contents <- liftIO $ runRubyString [prog, file']
--   liftIO $ writeUTF8File output $ unlines contents

--option--------------------------------------------------------------------------------------------
optionPrint :: Config -> String -> [KensinData] -> IO ()
optionPrint cfg sym kd = do
  let func | sym == "lady"   = ladies1P
           | sym == "camera" = cameraP
           | sym == "jinpai" = jinpaiP
  let list = sort (filterM func kd `runReader` cfg)
  normalPrint list `runReaderT` cfg
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

  case (count' opt, receipt' opt, meibo' opt, option' opt) of
    (True, _, _, _) -> jusin csv
    (_, True, _, _) -> do
      let s = receiptSunday csv `runReader` cfg
      let w = receiptWeekday csv `runReader` cfg
      putStrLn $ toReceipt s `runReader` cfg
      putStrLn $ toReceipt w `runReader` cfg
    (_, _, True, _) -> putStrLn (meiboOutput csv `runReader` cfg)
    (_, _, _, True) -> do
      optionPrint cfg "lady" csv
      putStrLn "--------------------------------------------------"
      optionPrint cfg "camera" csv
      putStrLn "--------------------------------------------------"
      optionPrint cfg "jinpai" csv
    (_, _, _, _) -> jusin csv
--Command Line Option-------------------------------------------------------------------------------
data Options = Options { count'    :: Bool
                       , receipt'  :: Bool
                       , meibo'    :: Bool
                       , base'     :: Bool
                       , option'   :: Bool
                       } deriving (Show)

countP :: O.Parser Bool
countP = O.switch $ O.short 'c' <> O.long "count" <> O.help "Count day by day."

receiptP :: O.Parser Bool
receiptP = O.switch $ O.short 'r' <> O.long "receipt" <> O.help "Output receipts."

meiboP :: O.Parser Bool
meiboP = O.switch $ O.short 'm' <> O.long "meibo" <> O.help "Output meibo."

baseP :: O.Parser Bool
baseP = O.switch $ O.short 'b' <> O.long "base" <> O.help "Output basic info."

optionP :: O.Parser Bool
optionP = O.switch $ O.short 'o' <> O.long "option" <> O.help "Output option data."


optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> countP
           <*> receiptP
           <*> meiboP
           <*> baseP
           <*> optionP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "Program for kensin."
    , O.header "kensin.exe -- program for kensin."
    , O.footer ""
    , O.progDesc ""
    ]
--test----------------------------------------------------------------------------------------------

