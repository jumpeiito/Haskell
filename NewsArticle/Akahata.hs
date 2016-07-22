module NewsArticle.Akahata (makePage, makeListedPage) where

import Strdt
import Data.Time
import Data.List
import Data.Monoid
import Data.Maybe (fromJust)
import NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Internal as Txi

makeListedPage :: Day -> ListedPage B.ByteString
makeListedPage d = LP base' d url' (newsList base')
  where base' = "http://www.jcp.or.jp/akahata/"
        url'  = makeTopPageURL d base'

newsList :: URL -> [TagTree B.ByteString] -> [URL]
newsList base = map (fullURL base) . extractHref . extractTree
  where extractTree = (findTree [(Always, Attr "newslist")] `concatMap`)
        extractHref = (findAttribute (B.pack "href")      `concatMap`)

fullURL :: URL -> B.ByteString -> URL
fullURL base url = makeURLFunction d base url'
  where url' = B.unpack url
        dstr = Data.List.take 8 url'
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
takeText = map stringFold . filter' . treeToStringList . makeTree
  where makeTree = concatMap $ findTree [(Name "p",  Always),
                                          -- (Name "h1", Always),
                                          -- (Name "h2", Always),
                                          (Name "h3", Always)]
        treeToStringList = map treeText
        filter' = filterBlankLines

makePage :: URL -> Page B.ByteString
makePage url = Page url takeTitle takeText

