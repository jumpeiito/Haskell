module Handler.MakeMeibo where

import           Meibo.Base             (meiboMain, telephoneStr, addressStr, Line (..))
import qualified Meibo.Base             as MB
import           Import

bunkaiList :: [String]
bunkaiList = ["全", "石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]

bunkaiHrefWidget :: WidgetT App IO ()
bunkaiHrefWidget = do
  toWidget [whamlet|
                   $forall bun <- bunkaiList
                      <a href=@{MakeMeiboR bun}>#{bun}
                   |]

concernP :: Line -> Bool
concernP = ("付" `isInfixOf`) . MB.exp

getMakeMeiboR :: String -> HandlerT App IO Html
getMakeMeiboR bunkai = do
  foo <- liftIO $ meiboMain bunkai
  let persons = zip [0..] foo :: [(Int, Line)]
  defaultLayout $ do
    bunkaiHrefWidget
    addScript $ StaticR js_MakeMeibo_js
    $(widgetFile "MakeMeibo")
