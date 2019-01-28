{-# LANGUAGE RecordWildCards #-}
module Snews.NewsArticle.Akahata ( config
                                 , makeNewsList
                                 ) where

import Util.Strdt                       (strdt, nendo, dayStrWithSep)
import Data.Time                        (Day (..))
import Data.Maybe                       (fromJust)
import Control.Monad.Reader
import Snews.NewsArticle.Base
import Text.StringLike                  (StringLike, castString)
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx

config = Con { hostName  = "http://www.jcp.or.jp/akahata/"
             , baseName  = "index.html"
             , rootAK    = [(Name "a", Attr "important") , (Name "a", Attr "normal")]
             , titleAK   = [(Name "title", Always)]
             , textAK    = [(Name "p", Always), (Name "h3", Always)]
             , findFunc  = findTreeS
             , direct    = (Name "a", Always, Skip) : normalDirection
             , urlRecipe = [ Host
                           , Str "aik"
                           , Slash (MDay (show . (`mod` 1000) . nendo))
                           , Slash (MDay (dayStrWithSep '-'))
                           , Base]}

type ConfigReader a = Reader (Config [TagTree Tx.Text]) a

generateURL :: Day -> String -> ConfigReader String
generateURL d url = do
  let change conf = conf { urlRecipe = init (urlRecipe conf) ++ [Str url] }
  change `local` makeURL d

makeNewsList :: StringLike a => [TagTree a] -> [String]
makeNewsList tree =
  (`runReader` config) $
    ask >>= \Con {..} -> do
      mapM fullURL (castString "href" <~~ rootAK <~ tree)

fullURL :: StringLike a => a -> ConfigReader String
fullURL url = do
  let dstr = castString url
  let d    = fromJust $ strdt (take 8 dstr)
  generateURL d dstr
