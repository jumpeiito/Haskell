import Strdt
import NewsArticle.Base
import qualified NewsArticle.Akahata as Ak
import qualified NewsArticle.Common  as Cm
import Control.Monad
import Data.Time
import Data.Monoid
import Text.Printf
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Network.HTTP as Net
import qualified Util         as U
import qualified System.IO    as I
import qualified Data.Text.IO       as Txio
import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString.Char8 as B
----------------------------------------------------------------------------------------------------
orgdir = "f:/org/news/"
----------------------------------------------------------------------------------------------------
orgName :: Day -> String
orgName d = orgdir <> printf "%d%02d.org" y m
  where y = toYear d
        m = toMonth d

makeOrgUrl :: Day -> String
makeOrgUrl d = orgurl <> (dayStr8 d) <> "-1.html"
----------------------------------------------------------------------------------------------------
testIO2 :: [String] -> IO ()
testIO2 s = 
  U.withAppendFile "./test.org" $ \handle -> mapM_ (I.hPutStrLn handle) s
----------------------------------------------------------------------------------------------------
appendText :: [String] -> Day -> IO ()
appendText txt day' =
  U.withAppendFile orgfile $ \handle ->
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
main = do
  I.hSetEncoding I.stdout I.utf8
  td   <- todayDay
  -- let akahata = makeAkahata (fromGregorian 2016 7 1)
----------------------------------------------------------------------------------------------------
  let common = Cm.makeListedPage td
  cmpage <- getPageContents (topURL common)
  let pages = pageF common cmpage
  forM_ pages $ \page -> do
    B.putStrLn $ Cm.getTitle page
    mapM_ Txio.putStrLn $ Cm.getText page
----------------------------------------------------------------------------------------------------
  let akahata = Ak.makeListedPage td
  page <- getPageContents (topURL akahata)
  let urls = (urlF akahata) page
  let akpage = Ak.makePage ""
  forM_ urls $ \url -> do
    cont <- getPageContents url
    B.putStrLn $ (titleFunc akpage) cont
    mapM_ Txio.putStrLn $ (textFunc akpage) cont
----------------------------------------------------------------------------------------------------
  -- cont <- getPageContents testurl
  -- mapM_ B.putStrLn $ (textFunc akpage) cont
  -- pages <- [ getPageContents u >>= (return . textFunc akpage) | u <- urls ]
  -- return $ pages
  -- I.putStrLn (topPageURL td akahata)
----------------------------------------------------------------------------------------------------
testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "[読売新聞] 震災遺構の(保存)　合意形成へ議論を尽くそう (2015年08月24日)"), TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]]

testtitle = "[読売新聞] 震災遺構の保存　合意形成へ(議論)を尽くそう (2015年08月24日)"

---------- url -> ListedPage -> [url, url, url, ...] -> [Page, Page, Page, ...] -> [Article, Article, Article, ...]
---------- String -> ListedPage -> IO [String] -> IO [Page] -> IO [Article]

