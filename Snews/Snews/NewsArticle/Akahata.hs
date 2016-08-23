module Snews.NewsArticle.Akahata (makePage, makeListedPage,
                           takeTitle, takeText) where

import Util.Strdt                       (strdt, nendo, dayStrWithSep)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Data.Maybe                       (fromJust)
import Data.Text.Internal               (Text (..))
import Snews.NewsArticle.Base
import Text.StringLike                  (StringLike, castString)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx

makeListedPage :: StringLike a => Day -> ListedPage a
makeListedPage d = LP base' d url' (newsList base') (const [])
  where base' = "http://www.jcp.or.jp/akahata/"
        url'  = makeTopPageURL d base'

newsList :: StringLike a => URL -> [TagTree a] -> [URL]
newsList base = map (fullURL base) . extractHref . extractTree
  where extractTree = (findTree [(Name "a", Attr "important"),
                                 (Name "a", Attr "normal")] `concatMap`)
        extractHref = (findAttribute (castString "href")      `concatMap`)

fullURL :: StringLike a => URL -> a -> URL
fullURL base url = makeURLFunction d base url'
  where url' = castString url
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

takeTitle :: (Monoid a, StringLike a) => [TagTree a] -> Text
takeTitle tr = orgStar <> treeTextMap tree'
  where tree'   = ([(Name "title", Always)] ==>) `concatMap` tr
        orgStar = castString "** "
        treeTextMap = utf8Text . mconcat . map treeText

takeText :: (Monoid a, StringLike a) => [TagTree a] -> [Text]
takeText = map ((<> Tx.pack "\n") . stringFoldBase) .
           filterBlankLines                         .
           toString                                 .
           makeTree
  where makeTree = concatMap $ findTree [(Name "p",  Always),
                                          -- (Name "h1", Always),
                                          -- (Name "h2", Always),
                                          (Name "h3", Always)]
        toString      = map (treeTextEx directionList)
        directionList = (Name "a", Always, Skip) : normalDirection

makePage :: (Monoid a, StringLike a) => URL -> Page a
makePage url = Page url vacant takeTitle takeText
  where vacant = TagLeaf (TagText mempty)

