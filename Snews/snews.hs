module Main where

import           Util                           (withAppendFile
                                                , withOutFile
                                                , readUTF8
                                                , runFile
                                                , FileSystem (..))
import           Util.Strdt                     (strdt, toYear, toMonth, todayDay, dayStr6)
import           Snews.OrgParse                 (tagsOutput1)
import           Snews.OrgConduit               (parseToDayList)
import           Snews.NewsArticle.Base
import           Control.Monad.Reader
import           Control.Concurrent.Async
import           Network.HTTP                   (simpleHTTP, getRequest, getResponseBody)
import           Network.HTTP.Req
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
--------------------------------------------------
data StandardOutput
data FileOutput
data Encoding = UTF8 | SJIS
type Output a = ReaderT (Config a) IO ()

convertUTF8 :: String -> IO B.ByteString
convertUTF8 s = do
  utf8 <- C.open "utf8" (Just False)
  sjis <- C.open "cp932" (Just False)
  return $ C.fromUnicode utf8 $ C.toUnicode sjis $ B.pack s

noconvertUTF8 :: String -> IO B.ByteString
noconvertUTF8 = return . castString

getResponse :: String -> IO String
getResponse url = getResponseBody =<< simpleHTTP (getRequest url)

getPageContents ::
  (String-> IO B.ByteString) -> String -> IO [TagTree Tx.Text]
getPageContents f url = do
  http      <- simpleHTTP $ getRequest url
  body      <- getResponseBody http
  converted <- castString <$> f body
  return $ translateTags converted

getPageConvert, getPageNoConvert ::
  Day -> ReaderT (Config a) IO [TagTree Tx.Text]
getPageConvert d = do
  url <- makeURL d
  liftIO $ getPageContents convertUTF8 url
getPageNoConvert d = do
  url <- makeURL d
  liftIO $ getPageContents noconvertUTF8 url
--------------------------------------------------
printPage :: (Tx.Text -> IO b) -> Day -> a -> Output a
printPage f date page = do
  let printer = liftIO . f
  printer . Tx.pack $ "* " <> show date
  takeTitle page >>= printer
  takeText page  >>= mapM_ printer

standardOutput :: Day -> a -> Output a
standardOutput = printPage Txio.putStrLn

fileOutput :: FilePath -> Day -> a -> Output a
fileOutput fp = printPage $ outputter fp
  where
    outputter fp contents =
      withAppendFile fp $ \handle -> Txio.hPutStrLn handle contents

orgDirectory :: IO (Maybe FilePath)
orgDirectory = runFile $ Directory [ "c:/Users/Jumpei/org/news/"
                                   , "d:/home/Org/news/"]

verboseDayMakerOutput :: Day -> FilePath -> Output a
verboseDayMakerOutput d fp = liftIO $ do
  I.putStrLn $ "Output " ++ show d ++ " article --> " ++ fp
  I.hFlush I.stdout

dayMaker :: Bool -> Day -> IO ()
dayMaker bp td = do
  --(deciding output destination)-------------------------
  Just orgdir <- orgDirectory
  let orgFile = orgdir ++ dayStr6 td ++ ".org"

  let destination
        | bp        = standardOutput td
        | otherwise = fileOutput orgFile td

  (`runReaderT` Cm.config) $ do
    promise  <- liftIO . async . return =<< getPageConvert td
    contents <- liftIO $ wait promise
    forM_ (Cm.makeTree contents) destination

  (`runReaderT` Ak.config) $ do
    urls    <- Ak.makeNewsList <$> getPageNoConvert td
    promise <- forM urls (liftIO . async . getPageContents noconvertUTF8)
    forM_ promise $ \pr -> do
      contents <- liftIO $ wait pr
      destination contents

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
                         (Nothing, Nothing) -> do
                           let (y, m) = (toYear td, toMonth td)
                           -- 指定した年月のOrgファイルをパー
                           dlist      <- parseToDayList y m
                           forM_ dlist makeF
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

test = do
  setUTF
  td <- todayDay
  let key = [(Name "li", Attr "newslist")]
  (`runReaderT` Ak.config) $ do
    -- contents <- getPageNoConvert td
    -- contents <- getPageConvert td
    url <- makeURL td
    contents <- liftIO $ getResponse url
    let trees = concat $ map (findTree key) $ translateTags contents
    mapM_ (liftIO . I.putStrLn . textExtract) trees

-- test2 :: IO [TagTree Tx.Text]
test2 = do
  setSJIS
  fileContents <- (readUTF8 "index.html" :: IO Tx.Text)
  let key = [(Name "li", Attr "newslist")]
  let trees = concat $ map (findTree key) $ translateTags fileContents
  mapM_ (Txio.putStrLn . textExtract) trees

setSJIS = I.hSetEncoding I.stdout =<< I.mkTextEncoding "cp932"
setUTF  = I.hSetEncoding I.stdout I.utf8

url = "http://www.jcp.or.jp/akahata/aik18/2019-01-31/index.html"
