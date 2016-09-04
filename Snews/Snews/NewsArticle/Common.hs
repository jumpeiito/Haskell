module Snews.NewsArticle.Common ( dailyURL
                                , config
                                , makeTree
                                ) where

import Util.Strdt                       (dayStr8)
import Data.Time                        (Day (..))
import Data.Monoid                      ((<>))
import Data.Text.Internal               (Text (..))
import Control.Monad.Reader
import Snews.NewsArticle.Base
import Text.StringLike                  (StringLike, castString)
import Text.HTML.TagSoup.Tree
import qualified Data.Text              as Tx
----------------------------------------------------------------------------------------------------
config :: Config (TagTree Text)
config = Con { hostName = "http://shasetsu.seesaa.net/archives/"
             , baseName = "-1.html"
             , rootAK   = [(Name "div", Attr "blogbody")]
             , titleAK  = [(Name "h3", Attr "title")]
             , textAK   = [(Name "div", Attr "text")]
             , findFunc = findTree
             , direct   = normalDirection }

type ConfigReader a = Reader (Config (TagTree Text)) a

dailyURL :: Day -> ConfigReader String
dailyURL d = do
  host <- hostName <$> ask
  base <- baseName <$> ask
  return $ host <> dayStr8 d <> base

makeTree :: [TagTree Text] -> ConfigReader [TagTree Text]
makeTree b = flip findTreeS b <$> rootAK <$> ask

