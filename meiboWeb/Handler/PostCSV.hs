{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.PostCSV where

import Import
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

postPostCSVR :: String -> Handler Html
postPostCSVR parameters = do
  let (bunkai, indexes) = parameterInfo parameters
  datalist <- liftIO $ meiboMain bunkai

  param <- reqGetParams <$> getRequest
  numVal :: Int <- runInputPost $ ireq intField "count1"

  let test = show param

  defaultLayout $(widgetFile "postcsv")
    
