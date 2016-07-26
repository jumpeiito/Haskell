module NewsArticle.Akahata (makePage, makeListedPage,
                           takeTitle, takeText) where

import Strdt                            (strdt, nendo, dayStrWithSep)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Data.Maybe                       (fromJust)
import Data.ByteString.Char8            (ByteString (..), pack, unpack)
import Data.Text.Internal               (Text (..))
import NewsArticle.Base
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx
import qualified Text.StringLike        as Like

makeListedPage :: Like.StringLike a => Day -> ListedPage a
makeListedPage d = LP base' d url' (newsList base') (const [])
  where base' = "http://www.jcp.or.jp/akahata/"
        url'  = makeTopPageURL d base'

newsList :: Like.StringLike a => URL -> [TagTree a] -> [URL]
newsList base = map (fullURL base) . extractHref . extractTree
  where extractTree = (findTree [(Always, Attr "newslist")] `concatMap`)
        extractHref = (findAttribute (Like.castString "href")      `concatMap`)

fullURL :: Like.StringLike a => URL -> a -> URL
fullURL base url = makeURLFunction d base url'
  where url' = Like.castString url
        dstr = take 8 url'
        d    = fromJust $ strdt dstr

makeTopPageURL :: Day -> URL -> URL
makeTopPageURL day base = makeURLFunction day base "index.html"

makeURLFunction :: Day -> URL -> URL -> URL
makeURLFunction day base subpage =
  base <> "aik" <> nendo' </> day' </> subpage
  where (</>) a = (a <>) . ("/" <>)
        nendo'  = show $ (`mod` 1000) $ nendo day
        day'    = dayStrWithSep '-' day

takeTitle :: (Monoid a, Like.StringLike a) => [TagTree a] -> Text
takeTitle tr = orgStar <> treeTextMap tree'
  where tree'   = ([(Name "title", Always)] ==>) `concatMap` tr
        orgStar = Like.castString "** "
        treeTextMap = utf8Text . mconcat . map treeText

takeText :: (Monoid a, Like.StringLike a) => [TagTree a] -> [Text]
takeText = map (<> Tx.pack "\n") .
           map stringFold        .
           filterBlankLines      .
           toString              .
           makeTree
  where makeTree = concatMap $ findTree [(Name "p",  Always),
                                          -- (Name "h1", Always),
                                          -- (Name "h2", Always),
                                          (Name "h3", Always)]
        toString      = map (treeTextEx directionList)
        directionList = (Name "a", Always, Skip) : normalDirection

makePage :: (Monoid a, Like.StringLike a) => URL -> Page a
makePage url = Page url vacant takeTitle takeText
  where vacant = TagLeaf (TagText mempty)

