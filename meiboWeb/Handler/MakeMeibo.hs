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
                      <a href="/makeMeibo/#{bun}">#{bun}
                   <br>
                   |]

buttonMakeCSV bunkai = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function getCheckbox () {
                         var ret = "#{bunkai}&";
                         var table = document.getElementById("MainTable")
                         for (var i = 0; i < table.rows.length; i++) {
                            if (document.forms.CheckBox[i].checked == true) {
                               ret = ret + String(i) + "&";
                            }
                         }
                         window.open("/csv/" + ret, '_blank');
                      }
                   |]

checkFunc = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function checkFunc (obj, row) {
                        var table = document.getElementById("MainTable")
                        for (var i = 0; i < table.rows.length; i++) {
                          var row = table.rows[i];
                          if (document.forms.CheckBox[i].checked == true) {
                            row.style.backgroundColor = "pink";
                          } else {
                            row.style.backgroundColor = "white";
                          }
                        }
                      }
                   |]

releaseFunc = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function release () {
                        var table = document.getElementById("MainTable")
                        for (var i = 0; i < table.rows.length; i++) {
                          document.forms.CheckBox[i].checked = false;
                          table.rows[i].style.backgroundColor = "white";
                        }
                      }
                   |]

concernP :: Line -> Bool
concernP = ("付" `isInfixOf`) . MB.exp

getMakeMeiboR :: String -> HandlerT App IO Html
getMakeMeiboR hoge = do
  -- ((res, widget), enc) <- runFormGet carForm
            -- <form enctype=#{enc}>
            --    ^{widget}
  -- window.open(@{CSVR} + "/#{hoge}," + ret);
  foo <- liftIO $ meiboMain hoge
  let persons = zip [0..] foo :: [(Int, Line)]
  defaultLayout $ do
    buttonMakeCSV hoge
    bunkaiHrefWidget
    checkFunc
    releaseFunc
    [whamlet|
            <input type="button" onClick="getCheckbox();" value="CSV作成">
            <input type="button" onClick="release();" value="全選択解除">
            <form #CheckBox>
              <table border=1 #MainTable>
                $forall (n, i) <- persons
                  <tr #tr#{n}>
                    <td class="group#{n}"><input type="checkbox" id="check#{n}" onclick="checkFunc(this, #{n})">
                    <td class="group#{n}">#{n}
                    <td id="NameID#{n}" class="group#{n}">#{name i}
                    <td class="group#{n}">
                      <a href="https://maps.google.co.jp/maps?q=#{addressStr i}">#{addressStr i}
                    <td id="TelID#{n}" class="group#{n}">#{telephoneStr i}
            |]
