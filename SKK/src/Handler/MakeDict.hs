module Handler.MakeDict where

import Import

getMakeDictR :: Handler Html
getMakeDictR = do
  defaultLayout $ do
    liftIO $ Prelude.print "hoge"

postMakeDictR :: Handler Html
postMakeDictR = error "Not yet implemented: postMakeDictR"
