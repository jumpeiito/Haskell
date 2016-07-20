import Strdt
import NewsArticle
import Data.Time
import Data.List
import Data.Text hiding (map, concatMap)
import Data.Text.Encoding
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Network.HTTP as Net
import qualified Util         as U
import qualified System.IO    as I
import qualified Control.Monad.State as St
import Control.Applicative    hiding (many, (<|>))
import Control.Monad.Writer
import System.Process
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Codec.Binary.UTF8.String
import Data.Text.ICU.Convert as C
import Data.ByteString.Char8 as B  hiding (concatMap, map)
-- cabal install text-icu --extra-include-dirs=f:/Haskell/icu/include/ --extra-lib-dirs=f:/Haskell/icu/lib/
----------------------------------------------------------------------------------------------------
orgdir = "f:/org/news/"
orgurl = "http://shasetsu.seesaa.net/archives/"
----------------------------------------------------------------------------------------------------
orgName :: Day -> String
orgName d = orgdir <> printf "%d%02d.org" y m
  where y = toYear d
        m = toMonth d

makeOrgUrl :: Day -> String
makeOrgUrl d = orgurl <> (dayStr8 d) <> "-1.html"
----------------------------------------------------------------------------------------------------
translateTags :: ByteString -> [TagTree ByteString]
translateTags str = tagTree $ parseTags str
----------------------------------------------------------------------------------------------------
extractBlogBody :: [TagTree ByteString] -> [TagTree ByteString]
extractBlogBody =
  concatMap $ findTree (Always, Attr "blogbody")
----------------------------------------------------------------------------------------------------
-- testIO = do
--   contents <- U.readUTF8File "f:/haskell/2.html"
--   I.hSetEncoding I.stdout I.utf8
--   let tra = translateTags contents
--   return $ map makeArticle $ extractBlogBody tra

-- http://www.jcp.or.jp/akahata/aik16/2016-07-18/index.html
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
convertUTF8 :: String -> IO ByteString
convertUTF8 s = do
  utf8 <- C.open "utf8" (Just False)
  sjis <- C.open "cp932" (Just False)
  return $ C.fromUnicode utf8 (C.toUnicode sjis (B.pack s))

getPageContents :: String -> IO [TagTree ByteString]
getPageContents url = do
  http      <- Net.simpleHTTP (Net.getRequest url)
  body      <- Net.getResponseBody http
  converted <- convertUTF8 body
  return $ translateTags converted
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  td   <- todayDay
  let akahata = LP "http://www.jcp.or.jp/akahata/" akahataMakeURL akahataNewsList
  page <- getPageContents (topPageURL td akahata)
  mapM_ B.putStrLn $ (listedKey akahata) page
  -- I.putStrLn (topPageURL td akahata)
----------------------------------------------------------------------------------------------------
testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "[読売新聞] 震災遺構の(保存)　合意形成へ議論を尽くそう (2015年08月24日)"), TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]]

testtitle = "[読売新聞] 震災遺構の保存　合意形成へ(議論)を尽くそう (2015年08月24日)"

---------- url -> ListedPage -> [url, url, url, ...] -> [Page, Page, Page, ...] -> [Article, Article, Article, ...]
---------- String -> ListedPage -> IO [String] -> IO [Page] -> IO [Article]
