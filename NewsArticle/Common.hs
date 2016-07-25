module NewsArticle.Common (makeListedPage) where

import Util                             (withAppendFile)
import Strdt                            (dayStr8)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Data.Maybe                       (fromJust)
import Data.Text.Internal               (Text (..))
import NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Data.ByteString.Char8  as B
import qualified Data.Text              as Tx
import qualified Text.StringLike        as Like

----------------------------------------------------------------------------------------------------
import System.Process
import qualified Data.Text.IO as Txio
import qualified System.IO as I
----------------------------------------------------------------------------------------------------

makeListedPage :: (Monoid a, Like.StringLike a) => Day -> ListedPage a
makeListedPage d = LP base' d url' (const []) extractPage
  where base' = "http://shasetsu.seesaa.net/archives/"
        url'  = base' <> dayStr8 d <> "-1.html"

extractPage :: (Monoid a, Like.StringLike a) => [TagTree a] -> [Page a]
extractPage tr = flip map (makeTree tr) $ \n ->
  Page mempty n takeTitle takeText
  where makeTree = reverse . 
                   concatMap (findTree [(Name "div", Attr "blogbody")])

takeTitle :: (Monoid a, Like.StringLike a) => [TagTree a] -> a
takeTitle = (Like.castString "** " <>) . treeTextMap . makeTree
  where treeTextMap = mconcat . map treeText
        makeTree    = concatMap $ findTree [(Name "h3", Attr "title")]

takeText :: (Monoid a, Like.StringLike a) => [TagTree a] -> [Text]
takeText = map (<> Tx.pack "\n")               .
           map stringFold                      .
           filterBlankLines                    .
           concatMap (lines . Like.castString) .
           map treeText                        .
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
  withAppendFile "f:/Haskell/NewsArticle/test" $ \handle -> do
    I.hSeek handle I.AbsoluteSeek 10
    I.hPutStrLn handle "** testoutput **"
