import           Util                           (withAppendFile, readUTF8File)
import           Util.Strdt                     (strdt, toYear, toMonth, todayDay)
import           Snews.OrgParse                 (parseToDayList)
import           Snews.NewsArticle.Base
import           Control.Monad                  (forM, forM_, when)
import           Control.Concurrent.Async
import           Data.Time                      (Day (..))
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           Text.Printf                    (printf)
import           Text.HTML.TagSoup.Tree
import qualified Options.Applicative            as O
import qualified Snews.NewsArticle.Akahata      as Ak
import qualified Snews.NewsArticle.Common       as Cm
import qualified Network.HTTP                   as Net
import qualified System.IO                      as I
import qualified Data.Text.IO                   as Txio
import qualified Data.Text.ICU.Convert          as C
import qualified Data.ByteString.Char8          as B
----------------------------------------------------------------------------------------------------
convertUTF8 :: String -> IO B.ByteString
convertUTF8 s = do
  utf8 <- C.open "utf8" (Just False)
  sjis <- C.open "cp932" (Just False)
  return $ C.fromUnicode utf8 $ C.toUnicode sjis $ B.pack s

getPageContents :: String -> IO [TagTree B.ByteString]
getPageContents url = do
  http      <- Net.simpleHTTP $ Net.getRequest url
  body      <- Net.getResponseBody http
  converted <- convertUTF8 body
  return $ translateTags converted
----------------------------------------------------------------------------------------------------
printer f1 f2 page = do
  Txio.putStrLn $ f1 page
  mapM_ Txio.putStrLn $ f2 page

dayMaker :: Day -> IO ()
dayMaker td = do
  --(definition)-----------------------------------------
  I.putStrLn $ "* " <> show td
  let common  = Cm.makeListedPage td
  let akahata = Ak.makeListedPage td :: ListedPage B.ByteString
  let akpage  = Ak.makePage ""
  --(make a promise)--------------------------------------
  cmPromise  <- async $ getPageContents $ topURL common
  urlPromise <- async . return . urlF akahata =<< getPageContents (topURL akahata)
  --(common parts)----------------------------------------
  cmContents <- wait cmPromise
  forM_ (pageF common cmContents) $ printer getTitle getText
  --(akahata parts)---------------------------------------
  urls <- wait urlPromise
  conc <- forM urls (async . getPageContents)
  forM_ conc $ \asy -> do
    promise <- wait asy
    printer (titleFunc akpage) (textFunc akpage) promise

main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  td   <- todayDay
  --------------------------------------------------
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo
  --------------------------------------------------
  sjis <- I.mkTextEncoding "CP932"
  when (sjis' opt)  $ I.hSetEncoding I.stdout sjis
  --------------------------------------------------
  case (today' opt, force opt, date' opt) of
    (True, _, _) -> dayMaker td
    (_, f, d)    -> case (strdt f, strdt d) of
                      (Just f', _) -> dayMaker f'
                      (_, Just d') -> do
                        let (y, m) = (toYear d', toMonth d')
                        dlist <- parseToDayList y m
                        forM_ dlist dayMaker

data Options = Options { today' :: Bool,
                         date'  :: String,
                         sjis'  :: Bool,
                         force  :: String
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

forceP :: O.Parser String
forceP = O.strOption $ mconcat
        [ O.short 'f', O.long "force"
        , O.help "If given date (8 digit chars, not including hyphons), output articles."
        , O.metavar ""
        , O.value ""
        , O.showDefaultWith id]
        
sjisP :: O.Parser Bool
sjisP = O.switch $ O.short 's' <> O.long "sjis" <> O.help "Output with char-set sjis"

optionsP :: O.Parser Options
optionsP = (<*>) O.helper $ Options <$> todayP <*> dateP <*> sjisP <*> forceP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "test program."
    , O.header "snews.exe -- get a daily news article program."
    , O.footer ""
    , O.progDesc ""
    ]    
