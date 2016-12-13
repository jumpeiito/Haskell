{-# LANGUAGE OverloadedStrings #-}
module Handler.CSV where

import           Import
import           GHC.List               ((!!), init)
import           Util.StrEnum           (split)
import           Util.Telephone         (telString, Telephone (..))
import           Meibo.Base             (meiboMain, Line (..))
import           Text.Read

type Bunkai     = String
type LineNumber = Int

parameterInfo :: String -> (Bunkai, [LineNumber])
parameterInfo str = (bunkai, map read (init rest))
  where bunkai:rest = split '&' str

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

getCSVR :: String -> HandlerT App IO Html
getCSVR parameter = do
  let (bunkai, indexes) = parameterInfo parameter
  datalist <- liftIO $ meiboMain bunkai
  let persons = zip ([0..]::[Int]) $ map (datalist!!) indexes
  let yetpay = length indexes
  let mother = length datalist
  defaultLayout $ do
    addScript $ StaticR js_buttonChange_js
    [whamlet|
            <div .container>
              <ul .breadcrumb>
                <li .active>滞納者#{yetpay}人 現勢#{mother}人 推定納入率#{kumiaihiRatio yetpay mother}%|]
    [whamlet|
            <input type=button onClick="change(#{mother});" value="CSV作成">
            <input type=button onClick="curWrite();" value="書き出し">
            <input type=button onClick="test();" value="テスト">|]
    [whamlet|
            <table border=1 #kumiai>
              $forall (n, p) <- persons
                <tr .linear#{n}>
                  <form #form#{n}>
                    <td width=20pt .linear#{n}>#{han p}
                    <td width=120pt .linear#{n}>#{name p}
                    <td .linear#{n}><input type="number" maxlength=2 max=99 min=1 value=1 style="width:40px;" class="numbers" onchange="change(#{mother});">
                    $forall t <- telOnly p
                      $if (length $ telOnly p) > 2
                        <td .linear#{n}><input type="button" style="width:150px; border:thin" value=#{telString t} onclick="buttonChange(this);">
                      $else
                        <td .linear#{n}><input type="button" style="background:pink; width:150px; border:thin" value=#{telString t}>
                      
            |]
