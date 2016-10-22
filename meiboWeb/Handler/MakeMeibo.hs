module Handler.MakeMeibo where

-- import Util       (runRubyString)
-- import Util.Strdt (today)
-- import Util.Telephone (telString)
-- import Data.List  (intercalate)
import Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
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

buttonMakeCSV bunkai allList = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function getCheckbox () {
                         var ret = "";
                         for (var i = 0; i < #{length allList}; i++) {
                            if (document.forms.CheckBox[i].checked == true) {
                               ret = ret + String(i) + ",";
                            }
                         }
                         window.open("/csv/#{bunkai}," + ret, '_blank');
                      }
                   |]

getMakeMeiboR :: String -> HandlerT App IO Html
getMakeMeiboR hoge = do
  -- ((res, widget), enc) <- runFormGet carForm
            -- <form enctype=#{enc}>
            --    ^{widget}
  -- window.open(@{CSVR} + "/#{hoge}," + ret);
  foo <- liftIO $ meiboMain hoge
  let persons = zip [0..] foo :: [(Int, Line)]
  defaultLayout $ do
    bunkaiHrefWidget
    buttonMakeCSV hoge foo
    [whamlet|
            <input type="button" onClick="getCheckbox();" value="CSV作成">
            <form name="CheckBox">
               <table border=1>
                  $forall (n, i) <- persons
                     <tr id="#{n}">
                        <td><input type="checkbox" id="check#{n}">
                        <td id="num#{n}">#{n}
                        <td>#{name i}
                        <td>
                           <a href="https://maps.google.co.jp/maps?q=#{addressStr i}">#{addressStr i}
                        <td>#{telephoneStr i}
            |]
