module Handler.InterSection where

import Import

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

getCheckedBoxValue :: Handler [Int]
getCheckedBoxValue = do
  let labels = ["check" <> (Tx.pack $ show n) | n <- [0..1000]]
  catMaybes <$> mapM (runInputPost . iopt intField) labels

getParameter :: (Bunkai, [LineNumber]) -> String
getParameter (bun, n) = printf "%s&%s" bun $ concatMap ((++ "&") . show) n

getMeibo :: [Int] -> HandlerT App IO [Person]
getMeibo indexes = runDB $ do
  bkn <- selectList [PersonPid /<-. indexes] []
  return $ map entityVal bkn

postInterSectionR :: Bunkai -> Handler Html
postInterSectionR bunkai = do
  indexes <- getCheckedBoxValue

  persons <- getMeibo indexes

  
