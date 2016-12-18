{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.PostCSV where

import Import
import           GHC.List               ((!!), init)
import           Data.List              (intercalate)
import           Util.StrEnum           (split)
import           Util.Telephone         (telString, Telephone (..))
import           Meibo.Base             (meiboMain, Line (..))
import           Text.Read
import           Control.Arrow          ((&&&))
import           Control.Exception      (SomeException)
import qualified Data.Text              as Tx
-- import           Data.Maybe             (catMaybes)

type Bunkai     = String
type LineNumber = Int

data Person = P { name  :: Text
                , count :: Int
                , row   :: Int }

parameterInfo :: String -> (Bunkai, [LineNumber])
parameterInfo str = (bunkai, map read (init rest))
  where bunkai:rest = split '&' str

getButtonValueList :: Int -> Handler [Maybe Text]
getButtonValueList row = mapM (runInputPost . iopt textField) rows
  where rows      = [ "button" <> pk row <> "-" <> pk n | n <- [0..5]]
        pk        = Tx.pack . show

getButtonText :: Int -> Handler Text
getButtonText row = do
  vl <- getButtonValueList row
  return $ mconcat $ catMaybes vl

getIntField :: Int -> Handler Int
getIntField row = runInputPost $ ireq intField ("count" <> Tx.pack (show row))
  
getCSVStatus :: Int -> Handler (Int, Text)
getCSVStatus row = (,) <$> getIntField row
                       <*> getButtonText row

postPostCSVR :: String -> Handler Html
postPostCSVR parameters = do
  let (bunkai, indexes) = parameterInfo parameters
  datalist <- liftIO $ meiboMain bunkai
  
  let meibo = map (datalist!!) indexes

  info <- mapM getCSVStatus [0..(length indexes - 1)]

  let hoge = zip meibo info
  defaultLayout $(widgetFile "postcsv")
