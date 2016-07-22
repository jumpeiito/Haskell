module NewsArticle.Common where

import Strdt
import Data.Time
import Data.List
import Data.Monoid
import Data.Maybe (fromJust)
import NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Internal as Txi

       ----------------------------------------------------------------------------------------------------
import System.Process
----------------------------------------------------------------------------------------------------

makeListedPage :: Day -> ListedPage B.ByteString
makeListedPage d = LP base' d url' (const []) extractPage
  where base' = "http://shasetsu.seesaa.net/archives/"
        url'  = base' <> dayStr8 d <> "-1.html"

extractPage :: [TagTree B.ByteString] -> [Page B.ByteString]
extractPage = undefined

takeTitle :: [TagTree B.ByteString] -> B.ByteString
takeTitle = treeTextMap . (concatMap $ findTree [(Name "h3", Attr "title")])
  where treeTextMap = mconcat . map treeText

takeText :: [TagTree B.ByteString] -> [Txi.Text]
takeText = map stringFold . filter' . treeToStringList . makeTree
  where makeTree = concatMap $ findTree [(Name "div", Attr "text")]
        treeToStringList = map treeText
        filter' = filterBlankLines

testIO21 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./1.html"] Nothing Nothing
  page <- B.hGetContents p
  return $ translateTags page
