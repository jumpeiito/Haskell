module NewsArticle.Common (makeListedPage) where

import Strdt            (dayStr8)
import Data.Time        (Day (..))
import Data.Monoid      ((<>))
import Data.Maybe       (fromJust)
import NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Util                  as U
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as Tx
import qualified Data.Text.Internal    as Txi

----------------------------------------------------------------------------------------------------
import System.Process
import qualified Data.Text.IO as Txio
import qualified System.IO as I
----------------------------------------------------------------------------------------------------

makeListedPage :: Day -> ListedPage B.ByteString
makeListedPage d = LP base' d url' (const []) extractPage
  where base' = "http://shasetsu.seesaa.net/archives/"
        url'  = base' <> dayStr8 d <> "-1.html"

extractPage :: [TagTree B.ByteString] -> [Page B.ByteString]
extractPage tr = flip map (makeTree tr) $ \n ->
  Page mempty n takeTitle takeText
  where makeTree = reverse . 
                   concatMap (findTree [(Name "div", Attr "blogbody")])

takeTitle :: [TagTree B.ByteString] -> B.ByteString
takeTitle = (B.pack "** " <>) . treeTextMap . makeTree
  where treeTextMap = mconcat . map treeText
        makeTree    = concatMap $ findTree [(Name "h3", Attr "title")]

takeText :: [TagTree B.ByteString] -> [Txi.Text]
takeText = map (<> Tx.pack "\n") .
           map stringFold        .
           filterBlankLines      .
           concatMap B.lines     .
           map treeText          .
           makeTree
  where makeTree = concatMap $ findTree [(Name "div", Attr "text")]
----------------------------------------------------------------------------------------------------
testIO21 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./6.html"] Nothing Nothing
  page <- B.hGetContents p
  I.hSetEncoding I.stdout I.utf8
  -- let l = takeText $ translateTags page
  -- Txio.putStrLn (head l)
  -- print (head l)
  -- (B.putStrLn) $ takeTitle $ translateTags page
  return $ translateTags page

testIO22 = do
  U.withAppendFile "f:/Haskell/NewsArticle/test" $ \handle -> do
    I.hSeek handle I.AbsoluteSeek 10
    I.hPutStrLn handle "** testoutput **"
