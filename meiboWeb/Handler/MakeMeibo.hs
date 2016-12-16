module Handler.MakeMeibo where

-- import Util       (runRubyString)
-- import Util.Strdt (today)
-- import Util.Telephone (telString)
-- import Data.List  (intercalate)
import Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import qualified Meibo.Base as MB
import Import
-- import qualified System.IO              as I

bunkaiList :: [String]
bunkaiList = ["全", "石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]

bunkaiHrefWidget :: WidgetT App IO ()
bunkaiHrefWidget = do
  toWidget [whamlet|
                   $forall bun <- bunkaiList
                      <a href=@{MakeMeiboR bun}>#{bun}
                   |]

-- buttonMakeCSV bunkai = do
--   toWidget [whamlet|
--                    <script type="text/javascript">
--                       function getCheckbox () {
--                          var ret = "#{bunkai}&";
--                          var table = document.getElementById("MainTable")
--                          for (var i = 0; i < table.rows.length; i++) {
--                             if (document.forms.CheckBox[i].checked == true) {
--                                ret = ret + String(i) + "&";
--                             }
--                          }
--                          window.open("/csv/" + ret, '_blank');
--                       }
--                    |]

-- checkFunc = do
--   toWidget [whamlet|
--                    <script type="text/javascript">
--                       function checkFunc (obj, row) {
--                         var table = document.getElementById("MainTable")
--                         for (var i = 0; i < table.rows.length; i++) {
--                           var row = table.rows[i];
--                           if (document.forms.CheckBox[i].checked == true) {
--                             row.style.backgroundColor = "pink";
--                           } else {
--                             row.style.backgroundColor = "white";
--                           }
--                         }
--                       }
--                    |]

-- releaseFunc = do
--   toWidget [whamlet|
--                    <script type="text/javascript">
--                       function release () {
--                         var table = document.getElementById("MainTable")
--                         for (var i = 0; i < table.rows.length; i++) {
--                           document.forms.CheckBox[i].checked = false;
--                           table.rows[i].style.backgroundColor = "white";
--                         }
--                       }
--                    |]

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
