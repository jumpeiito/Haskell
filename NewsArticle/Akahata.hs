module NewsArticle.Akahata (makePage, makeListedPage,
                           takeTitle, takeText) where

import Strdt            (strdt, nendo, dayStrWithSep)
import Data.Time        (Day (..))
import Data.Monoid      ((<>))
import Data.Maybe       (fromJust)
import NewsArticle.Base
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.List             as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as Tx
import qualified Data.Text.Internal    as Txi

makeListedPage :: Day -> ListedPage B.ByteString
makeListedPage d = LP base' d url' (newsList base') (const [])
  where base' = "http://www.jcp.or.jp/akahata/"
        url'  = makeTopPageURL d base'

newsList :: URL -> [TagTree B.ByteString] -> [URL]
newsList base = map (fullURL base) . extractHref . extractTree
  where extractTree = (findTree [(Always, Attr "newslist")] `concatMap`)
        extractHref = (findAttribute (B.pack "href")      `concatMap`)

fullURL :: URL -> B.ByteString -> URL
fullURL base url = makeURLFunction d base url'
  where url' = B.unpack url
        dstr = L.take 8 url'
        d    = fromJust $ strdt dstr

makeTopPageURL :: Day -> URL -> URL
makeTopPageURL day base = makeURLFunction day base "index.html"

makeURLFunction :: Day -> URL -> URL -> URL
makeURLFunction day base subpage =
  base <> "aik" <> nendo' </> day' </> subpage
  where (</>) a = (a <>) . ("/" <>)
        nendo'  = show $ (`mod` 1000) $ nendo day
        day'    = dayStrWithSep '-' day

takeTitle :: [TagTree B.ByteString] -> B.ByteString
takeTitle tr = orgStar <> treeTextMap tree'
  where tree'   = ([(Name "title", Always)] ==>) `concatMap` tr
        orgStar = B.pack "** "
        treeTextMap = mconcat . map treeText

takeText :: [TagTree B.ByteString] -> [Txi.Text]
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

makePage :: URL -> Page B.ByteString
makePage url = Page url vacant takeTitle takeText
  where vacant = TagLeaf (TagText mempty)

