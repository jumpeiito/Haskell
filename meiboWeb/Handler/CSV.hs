{-# LANGUAGE OverloadedStrings #-}
module Handler.CSV where

import           Import
import           GHC.List               ((!!), init, head)
import           Util.StrEnum           (split)
import           Util.Telephone         (telString, Telephone (..))
import           Meibo.Base             (meiboMain, telephoneStr, addressStr, Line (..), output)
import qualified Meibo.Base             as MB
import           Text.Read

type Bunkai     = String
type LineNumber = Int

parameterInfo :: String -> (Bunkai, [LineNumber])
parameterInfo str = (bunkai, map read (init rest))
  where bunkai:rest = split '&' str

buttonDown = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function buttonDown (obj) {
                        alert(obj.style.border);
                        if (obj.style.border='none')
                          { obj.style.border='thin solid black'; }
                        else 
                          { obj.style.border='thin solid black'; }
                      }
                   |]

buttonChange = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function buttonChange (obj) {
                        if (obj.style.background) {
                          obj.style.background = null;
                        } else {
                          obj.style.background = 'pink';
                        }
                      }
                   |]


divChange dataList = do
  let current = length dataList
  toWidget [whamlet|
                   <script type="text/javascript">
                      function change () {
                        var divStr = document.getElementById("ratio").firstChild;
                        var yetpay = document.getElementById("yetpay").firstChild;
                        var inputs = document.getElementsByClassName("numbers");
                        var sum    = 0;
                        for (var i = 0; i < inputs.length; i++) {
                          sum = sum + Number(inputs.item(i).value);
                        }
                        ratio = Math.round((#{current} - sum) * 1000 / #{current}) / 10.0
                        divStr.data = "推定納入率" + String(ratio) + "%";
                        yetpay.data = "滞納者" + String(sum) + "人";
                      }
                   |]
    
exceptFax :: [Telephone] -> [Telephone]
exceptFax = filter faxFilter
  where faxFilter (Fax _) = False
        faxFilter _ = True

telOnly :: Line -> [Telephone]
telOnly = exceptFax . tel

telOnlyWithNum :: Line -> [(Int, Telephone)]
telOnlyWithNum = zip [0..] . telOnly

kumiaihiRatio :: Int -> Int -> Float
kumiaihiRatio yet allP = (((a - y) * 1000) / (10.0 * a))
  where (y, a) = (fromIntegral yet, fromIntegral allP)

infoPut dataList indexes = do
  toWidget [whamlet|
                   <div>
                   $with mother <- length dataList
                     $with yetpay <- length indexes
                       <div #yetpay>滞納者#{yetpay}人
                       <div #current>現勢#{mother}人
                       <div #ratio>推定納入率#{kumiaihiRatio yetpay mother}%
                   |]

writeButton = do
  toWidget [whamlet|
                   <script type="text/javascript">
                      function curWrite () {
                        var table = document.getElementById("kumiai");
                        var rowMax = table.rows.length
                        for (var i = 0; i < rowMax; i++) {
                          var line = table.getElementsByClassName("linear" + String(i))
                          var han = line.item(1).innerText;
                          var name = line.item(2).innerText;
                          var multiple = line.item(3).firstChild.value;
                          var tel = "";
                          for (var col = 4; col < 9; col++){
                            var column = line.item(col);
                            if (column) {
                              var but = column.firstChild;
                              if (but.style.background) {
                                tel = tel + column.firstChild.value + "・";
                              }
                            }
                          }
                          alert(han+name+multiple+tel);
                        }
                      }
                   |]

getCSVR :: String -> HandlerT App IO Html
getCSVR parameter = do
  let (bunkai, indexes) = parameterInfo parameter
  datalist <- liftIO $ meiboMain bunkai
  let persons = zip ([0..]::[Int]) $ map (datalist!!) indexes
  defaultLayout $ do
    buttonDown
    buttonChange
    divChange datalist
    infoPut datalist indexes
    writeButton
    [whamlet|
            <input type="button" onClick="change();" value="CSV作成">
            <input type="button" onClick="curWrite();" value="書き出し">
            <table border=1 #kumiai>
              $forall (n, p) <- persons
                <tr .linear#{n}>
                  <form #form#{n}>
                    <td width=20pt .linear#{n}>#{han p}
                    <td width=120pt .linear#{n}>#{name p}
                    <td .linear#{n}><input type="number" maxlength=2 max=99 min=1 value=1 style="width:50px;" class="numbers" onchange="change();">
                    $forall t <- telOnly p
                      $if (length $ telOnly p) > 2
                        <td .linear#{n}><input type="button" style="width:150px; border:thin" value=#{telString t} onclick="buttonChange(this);">
                      $else
                        <td .linear#{n}><input type="button" style="background:pink; width:150px; border:thin" value=#{telString t}>
                      
            |]
                      -- // $if (length $ telOnly p) > 2
                      -- //   <select name="selection" size="2" multiple>
                      -- //     $forall t <- telOnly p
                      -- //       <option value=#{telString t}>#{telString t}
                      -- // $else
                      -- //   #{intercalate "・" $ map telString $ telOnly p}
