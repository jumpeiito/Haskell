import           Util                           (withAppendFile, withOutFile)
import           Util.Strdt                     (strdt, toYear, toMonth, todayDay, dayStr6)
import           Snews.OrgParse                 (parseToDayList, tagsOutput1)
import           Snews.NewsArticle.Base
import           Control.Monad.Reader
import           Control.Concurrent.Async
import           Network.HTTP                   (simpleHTTP, getRequest, getResponseBody)
import           Data.Time                      (Day (..))
import           Data.Monoid                    ((<>))
import           Text.StringLike                (castString)
import           Text.HTML.TagSoup.Tree
import           System.Directory               (doesFileExist)
import qualified Options.Applicative            as O
import qualified Snews.NewsArticle.Akahata      as Ak
import qualified Snews.NewsArticle.Common       as Cm
import qualified System.IO                      as I
import qualified Data.Text                      as Tx
import qualified Data.Text.IO                   as Txio
import qualified Data.Text.ICU.Convert          as C
import qualified Data.ByteString.Char8          as B
----------------------------------------------------------------------------------------------------
convertUTF8 :: String -> IO B.ByteString
convertUTF8 s = do
  utf8 <- C.open "utf8" (Just False)
  sjis <- C.open "cp932" (Just False)
  return $ C.fromUnicode utf8 $ C.toUnicode sjis $ B.pack s

getPageContents :: String -> IO [TagTree Tx.Text]
getPageContents url = do
  http      <- simpleHTTP $ getRequest url
  body      <- getResponseBody http
  converted <- castString <$> convertUTF8 body
  return $ translateTags converted
----------------------------------------------------------------------------------------------------
filePrinter filename dt = 
  withAppendFile filename $ \handle ->
    Txio.hPutStrLn handle dt

printerCore outputF config page = do
  outputF $ takeTitle page `runReader` config
  mapM_ outputF $ takeText page `runReader` config

printer = printerCore Txio.putStrLn

fPrinter filename = printerCore $ filePrinter filename

dayMaker :: Bool -> Day -> IO ()
dayMaker bp td = do
  --(deciding output destination)-------------------------
  let orgFile = "d:/home/Org/news/" ++ dayStr6 td ++ ".org"
  bool <- doesFileExist orgFile
  
  let (trueOutput, headOutput)
        | bp        = (printer, Txio.putStrLn)
        | otherwise = (fPrinter orgFile, filePrinter orgFile)
  headOutput $ Tx.pack $ "* " <> show td
  I.putStrLn $ "Output " ++ show td ++ " article --> " ++ orgFile
  I.hFlush I.stdout
  --(make a promise)--------------------------------------
  let url = (makeURL td `runReader`)
  cmPromise  <- async $ getPageContents (url Cm.config)
  urlPromise <- async $ Ak.makeNewsList <$> getPageContents (url Ak.config)
  --(common parts)----------------------------------------
  cmContents <- wait cmPromise
  forM_ (Cm.makeTree cmContents) $ trueOutput Cm.config
  --(akahata parts)---------------------------------------
  urls <- wait urlPromise
  conc <- forM urls (async . getPageContents)
  forM_ conc $ \asy -> do
    promise <- wait asy
    trueOutput Ak.config promise

orgfileTagsOut :: IO ()
orgfileTagsOut = do
  str <- tagsOutput1
  withOutFile "f:/.tags" $ \handle ->
    I.hPutStrLn handle str

main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  td   <- todayDay
  --------------------------------------------------
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo
  --------------------------------------------------
  sjis <- I.mkTextEncoding "cp932"
  -- SJISで出力 (-s)
  when (sjis' opt)  $ I.hSetEncoding I.stdout sjis
  --------------------------------------------------
  let destination = output opt
  -- 標準出力に出力するか,ファイルに書き出すか。(-o)
  let makeF = dayMaker destination
  --------------------------------------------------
  case (tags opt, today' opt, force opt, date' opt) of
    -- 当日分の記事のみを取得。(-t)
    (True, _, _, _) -> orgfileTagsOut
    (_, True, _, _) -> makeF td
    (_, _, f, d)    -> case (strdt f, strdt d) of
                         -- 指定した日の記事を取得。(-f)
                         (Just f', _) -> makeF f'
                         -- 指定した年月のOrgファイルをパーズし,取得して
                         -- いない日付のものを取ってくる。(-d)
                         (_, Just d') -> do
                           let (y, m) = (toYear d', toMonth d')
                           -- 指定した年月のOrgファイルをパーズ
                           dlist      <- parseToDayList y m
                           forM_ dlist makeF

data Options = Options { today' :: Bool,
                         date'  :: String,
                         sjis'  :: Bool,
                         tags   :: Bool,
                         force  :: String,
                         output :: Bool
                       } deriving (Show)

todayP :: O.Parser Bool
todayP = O.switch $ O.short 't' <> O.long "today" <> O.help "Output today's article"

dateP :: O.Parser String
dateP = O.strOption $ mconcat
        [ O.short 'd', O.long "date"
        , O.help "If given year and month, Output news articles after parsing org files."
        , O.metavar "YEAR MONTH (6 digit chars)."
        , O.value ""
        , O.showDefaultWith id]

outputFileP :: O.Parser Bool
outputFileP = O.switch $ O.short 'o' <> O.long "stdout" <> O.help "Output to stdout."

forceP :: O.Parser String
forceP = O.strOption $ mconcat
        [ O.short 'f', O.long "force"
        , O.help "If given date (8 digit chars, not including hyphons), output articles."
        , O.metavar ""
        , O.value ""
        , O.showDefaultWith id]
        
sjisP :: O.Parser Bool
sjisP = O.switch $ O.short 's' <> O.long "sjis" <> O.help "Output with char-set sjis"

tagsP :: O.Parser Bool
tagsP = O.switch $ O.short 'g' <> O.long "tags" <> O.help "Output tags file"

optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> todayP
           <*> dateP
           <*> sjisP
           <*> tagsP
           <*> forceP
           <*> outputFileP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "test program."
    , O.header "snews.exe -- get a daily news article program."
    , O.footer ""
    , O.progDesc ""
    ]
