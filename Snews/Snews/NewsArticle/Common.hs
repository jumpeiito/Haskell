module Snews.NewsArticle.Common ( config
                                , makeTree
                                ) where

import Util.Strdt                       (dayStr8)
import Data.Text.Internal               (Text (..))
import Control.Monad.Reader
import Snews.NewsArticle.Base
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx
----------------------------------------------------------------------------------------------------
config :: Config (TagTree Text)
config = Con { hostName  = "http://shasetsu.seesaa.net/archives/"
             , baseName  = "-1.html"
             , rootAK    = [(Name "div", Attr "blogbody")]
             , titleAK   = [(Name "h3", Attr "title")]
             , textAK    = [(Name "div", Attr "text")]
             , findFunc  = findTree
             , direct    = normalDirection
             , urlRecipe = [Host, MDay dayStr8, Base]}

makeTree :: [TagTree Text] -> [TagTree Text]
makeTree b = (`runReader` config) $
  flip findTreeS b . rootAK <$> ask
