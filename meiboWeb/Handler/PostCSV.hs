{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.PostCSV where

import Import

postPostCSVR :: Handler Html
postPostCSVR = do
  param <- reqGetParams <$> getRequest
  numVal :: Int <- runInputPost $ ireq intField "count1"

  let test = show param

  defaultLayout $(widgetFile "postcsv")
    
