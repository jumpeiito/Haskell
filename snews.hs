import Util                             (withAppendFile)
import Strdt                            (strdt, toYear, toMonth, todayDay)
import OrgParse                         (parseToDayList)
import NewsArticle.Base
import Control.Monad                    (forM_)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Text.Printf                      (printf)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Options.Applicative    as O
import qualified NewsArticle.Akahata    as Ak
import qualified NewsArticle.Common     as Cm
import qualified Network.HTTP           as Net
import qualified System.IO              as I
import qualified Data.Text.IO           as Txio
import qualified Data.Text.ICU.Convert  as C
import qualified Data.ByteString.Char8  as B
----------------------------------------------------------------------------------------------------
orgdir = "f:/org/news/"
----------------------------------------------------------------------------------------------------
orgName :: Day -> String
orgName d = orgdir <> printf "%d%02d.org" y m
  where y = toYear d
        m = toMonth d

testIO2 :: [String] -> IO ()
testIO2 s = 
  withAppendFile "./test.org" $ \handle -> mapM_ (I.hPutStrLn handle) s
----------------------------------------------------------------------------------------------------
appendText :: [String] -> Day -> IO ()
appendText txt day' =
  withAppendFile orgfile $ \handle ->
  mapM_ (I.hPutStrLn handle) txt
  where orgfile = orgName day'
----------------------------------------------------------------------------------------------------
convertUTF8 :: String -> IO B.ByteString
convertUTF8 s = do
  utf8 <- C.open "utf8" (Just False)
  sjis <- C.open "cp932" (Just False)
  return $ C.fromUnicode utf8 (C.toUnicode sjis (B.pack s))

getPageContents :: String -> IO [TagTree B.ByteString]
getPageContents url = do
  http      <- Net.simpleHTTP (Net.getRequest url)
  body      <- Net.getResponseBody http
  converted <- convertUTF8 body
  return $ translateTags converted
----------------------------------------------------------------------------------------------------
-- main :: IO ()
-- printer :: Foldable t => (t1 -> B.ByteString)
--      -> (t1 -> t Txi.Text) -> t1 -> IO ()
-- printer titleF textF tree' = do
--   B.putStrLn $ titleF tree'
--   mapM_ Txio.putStrLn $ textF tree'

singleHTML tree = head tree'
  where tree' = findTree [(Name "body", Always)] `concatMap` tree

-- printer :: Page B.ByteString -> IO ()
printer f1 f2 page = do
  B.putStrLn $ f1 page
  mapM_ Txio.putStrLn $ f2 page

dayMaker :: Day -> IO ()
dayMaker td = do
  I.putStrLn $ "* " <> show td
  let common = Cm.makeListedPage td
  cmpage <- getPageContents (topURL common)
  let pages = pageF common cmpage
  forM_ pages (printer getTitle getText)
  ----------------------------------------------------------------------------------------------------
  let akahata = Ak.makeListedPage td
  page <- getPageContents (topURL akahata)
  let urls = (urlF akahata) page
  let akpage = Ak.makePage ""
  forM_ urls $ \url -> do
    -- cont <- singleHTML <$> getPageContents url
    -- let p = Page "" cont Ak.takeTitle Ak.takeText
    -- printer p
    cont <- getPageContents url
    printer (titleFunc akpage) (textFunc akpage) cont

main = do
  I.hSetEncoding I.stdout I.utf8
  td   <- todayDay
  --------------------------------------------------
  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo
  --------------------------------------------------
  case (today' opt, date' opt) of
    (True, _) -> dayMaker td
    (_, d)    -> case (strdt d :: Maybe Day) of
                   Just d' -> do
                     let (y, m) = (toYear d', toMonth d')
                     dlist  <- parseToDayList y m
                     forM_ dlist dayMaker
                   Nothing -> return ()

data Options = Options { today' :: Bool,
                         date'  :: String
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

optionsP :: O.Parser Options
optionsP = (<*>) O.helper $ Options <$> todayP <*> dateP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc "test program."
    , O.header "snews.exe -- get a daily news article program."
    , O.footer ""
    , O.progDesc ""
    ]    
