{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.GetQuery where

import           Import
import           Control.Arrow ((>>>))
import qualified Data.Text     as Tx
import qualified Data.Text.IO  as Tx

getGetQueryR :: Text -> Handler Text
getGetQueryR text = do
  query <- runDB $
             Prelude.map entityVal <$> selectList [SKKCandidate ==. text] []
  case query of
    []  -> return mempty
    h:_ -> return $ sKKQuery h

postGetQueryR :: Text -> Handler Html
postGetQueryR text = error "Not yet implemented: postGetQueryR"
