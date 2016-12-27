module Handler.InterSection where

import Import

import           Import
import           GHC.List               ((!!), init)
import           Util.StrEnum           (split)
import           Util.Telephone         (telParse, telString, Telephone (..))
import           Meibo.Base             (meiboMain, Line (..))
import           Text.Read
import           Text.Printf            (printf)
import qualified Data.Text              as Tx

type Bunkai     = String
type LineNumber = Int

data Counter = Counter { counter :: Int }

-- http://kurokawh.blogspot.jp/2014/09/haskellyesodsqlite-haskellquery.html

parameterInfo :: String -> (Bunkai, [LineNumber])
parameterInfo str = (bunkai, map read (init rest))
  where bunkai:rest = split '&' str

exceptFax :: [Telephone] -> [Telephone]
exceptFax = filter faxFilter
  where faxFilter (Fax _) = False
        faxFilter _ = True

telOnly :: Person -> [Telephone]
telOnly = exceptFax . telParse . personTel

telOnlyWithNum :: Person -> [(Int, Telephone)]
telOnlyWithNum = zip [0..] . telOnly

kumiaihiRatio :: Int -> Int -> Float
kumiaihiRatio yet allP = (((a - y) * 1000) / (10.0 * a))
  where (y, a) = (fromIntegral yet, fromIntegral allP)

getCheckedBoxValue :: Handler [Int]
getCheckedBoxValue = do
  let labels = ["check" <> (Tx.pack $ show n) | n <- [0..1000]]
  catMaybes <$> mapM (runInputPost . iopt intField) labels

getParameter :: (Bunkai, [LineNumber]) -> String
getParameter (bun, n) = printf "%s&%s" bun $ concatMap ((++ "&") . show) n

getMeibo :: [Int] -> HandlerT App IO [Person]
getMeibo indexes = runDB $ do
  bkn <- selectList [PersonPid <-. indexes] []
  return $ map entityVal bkn

postInterSectionR :: Bunkai -> Handler Html
postInterSectionR bunkai = do
  indexes <- getCheckedBoxValue

  meibo <- getMeibo indexes
  let persons = zip ([0..]::[Int]) $ meibo
  let yetpay = length indexes
  let mother = length meibo
  let parameter = getParameter (bunkai, indexes)

  defaultLayout $ do
    addScript $ StaticR js_buttonChange_js
    $(widgetFile "InterSection-header")
    $(widgetFile "InterSection-button")
    $(widgetFile "InterSection")
  
  
