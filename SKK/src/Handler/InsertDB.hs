{-# LANGUAGE OverloadedStrings #-}
module Handler.InsertDB where

import Import

getInsertDBR :: Text -> Handler ()
getInsertDBR text = do
  getParameters <- reqGetParams <$> getRequest
  let search = flip Prelude.lookup getParameters
  case (search "id", search "key") of
    (Just i, Just k) -> do
      runDB $ do
        query <- Prelude.map entityVal <$> selectList [SKKCandidate ==. i] []
        case query of
          []  -> insert $ SKK i k
          h:_ -> do
            deleteWhere [SKKCandidate ==. i]
            insert $ SKK i (sKKQuery h <> "/" <> k <> "/")
        return ()
    _ -> return ()

postInsertDBR :: Text -> Handler Html
postInsertDBR text = error "Not yet implemented: postInsertDBR"
