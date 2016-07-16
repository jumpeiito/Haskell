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
  concatMap (findTree always (("class", "blogbody")<@>))
----------------------------------------------------------------------------------------------------
-- testIO = do
--   contents <- U.readUTF8File "f:/haskell/2.html"
--   I.hSetEncoding I.stdout I.utf8
--   let tra = translateTags contents
--   return $ map makeArticle $ extractBlogBody tra

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
getPageContents :: String -> IO String
getPageContents url = 
  Net.simpleHTTP (Net.getRequest url) >>= Net.getResponseBody
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  page <- getPageContents (makeOrgUrl (fromGregorian 2016 7 11))
  sjis <- C.open "cp932" (Just False)
  utf8 <- C.open "utf8"  (Just False)
  let contents = (C.fromUnicode utf8 (C.toUnicode sjis (B.pack page)))
  -- (_, p, _, _) <- getPageContents (makeOrgUrl (fromGregorian 2016 7 10))
  -- I.hSetEncoding I.stdin I.utf8
  -- page <- B.hGetContents p
  -- I.hSetEncoding I.stdout I.utf8
  mapM_ (B.putStrLn . output . makeArticle) $ extractBlogBody $ translateTags contents
  -- putStrLn contents
  -- putStrLn =<< I.hGetContents p
  -- B.putStrLn contents
----------------------------------------------------------------------------------------------------
testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "[読売新聞] 震災遺構の(保存)　合意形成へ議論を尽くそう (2015年08月24日)"), TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]]

testtitle = "[読売新聞] 震災遺構の保存　合意形成へ(議論)を尽くそう (2015年08月24日)"
