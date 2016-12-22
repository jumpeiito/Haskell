{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.CSV where

import           Import
import           GHC.List               ((!!), init)
import           Util.StrEnum           (split)
import           Util.Telephone         (telString, Telephone (..))
import           Meibo.Base             (meiboMain, Line (..))
import           Text.Read
import           Text.Printf            (printf)
import qualified Data.Text              as Tx

type Bunkai     = String
type LineNumber = Int

data Counter = Counter { counter :: Int }

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

getCheckedBoxValue :: Int -> Handler [Int]
getCheckedBoxValue maxRow = do
  let labels = ["check" <> (Tx.pack $ show n) | n <- [0..maxRow - 1]]
  catMaybes <$> mapM (runInputPost . iopt intField) labels

getParameter :: (Bunkai, [LineNumber]) -> String
getParameter (bun, n) = printf "%s&%s" bun $ concatMap ((++ "&") . show) n

postCSVR :: String -> Handler Html
postCSVR bunkai = do
  param <- reqGetParams <$> getRequest

  datalist <- liftIO $ meiboMain bunkai

  let len = length datalist
  indexes <- getCheckedBoxValue len
  let persons = zip ([0..]::[Int]) $ map (datalist!!) indexes
  let yetpay = length indexes
  let mother = length datalist
  let parameter = getParameter (bunkai, indexes)

  defaultLayout $ do
    addScript $ StaticR js_buttonChange_js
    $(widgetFile "CSV-header")
    $(widgetFile "CSV-button")
    $(widgetFile "CSV")

getCSVR :: String -> HandlerT App IO Html
getCSVR parameter = do
  param <- reqGetParams <$> getRequest

  let (bunkai, indexes) = parameterInfo parameter
  datalist <- liftIO $ meiboMain bunkai
  let persons = zip ([0..]::[Int]) $ map (datalist!!) indexes
  let yetpay = length indexes
  let mother = length datalist

  defaultLayout $ do
    addScript $ StaticR js_buttonChange_js
    $(widgetFile "CSV-header")
    $(widgetFile "CSV-button")
    $(widgetFile "CSV")
