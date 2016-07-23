import Strdt
import NewsArticle.Base
import qualified NewsArticle.Akahata   as Ak
import qualified NewsArticle.Common    as Cm
import Control.Monad
import Data.Time
import Data.Monoid
import Text.Printf
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Network.HTTP          as Net
import qualified Util                  as U
import qualified System.IO             as I
import qualified Data.Text.IO          as Txio
import qualified Data.Text.Internal    as Txi
import qualified Data.Text.ICU.Convert as C
import qualified Data.ByteString.Char8 as B
----------------------------------------------------------------------------------------------------
orgdir = "f:/org/news/"
----------------------------------------------------------------------------------------------------
orgName :: Day -> String
orgName d = orgdir <> printf "%d%02d.org" y m
  where y = toYear d
        m = toMonth d

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
  -- dayMaker (fromGregorian 2016 7 21)
  dayMaker td
  -- let akahata = makeAkahata (fromGregorian 2016 7 1)
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
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

