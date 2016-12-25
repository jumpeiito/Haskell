{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Handler.TelFind where

import Import
import Meibo.Base               (meiboMain, Line (..), Key (..), telephoneStr, addressStr)
import qualified Data.Text              as Tx

type Query = String

postTelFindR :: Handler Html
postTelFindR = do
  datalist <- liftIO $ meiboMain "全"

  Just query <- runInputPost $ iopt textField "query"

  let targets = filter ((Tx.unpack query `isInfixOf`) . telephoneStr) datalist
  let persons = zip [0..] targets :: [(Int, Line)]
  let bunkai  = "全"
  defaultLayout $(widgetFile "TelFind")
