module Snews.NewsArticle.Common (makeListedPage) where

import Util.Strdt                       (dayStr8)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Data.Text.Internal               (Text (..))
import Snews.NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx
import qualified Text.StringLike        as Like
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

takeTitle :: (Monoid a, Like.StringLike a) => [TagTree a] -> Text
takeTitle = (utf8Text "** " <>) . treeTextMap . makeTree
  where treeTextMap = utf8Text . mconcat . map treeText
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
