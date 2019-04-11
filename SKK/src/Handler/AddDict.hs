{-# LANGUAGE OverloadedStrings #-}
module Handler.AddDict where

import Import
import Util
import qualified Prelude as P
import qualified Data.Text as Tx

lineToSKKData :: Text -> Maybe SKK
lineToSKKData skkdictline =
  case Tx.split (==' ') skkdictline of
    [cd, q] -> Just $ SKK cd q
    _       -> Nothing

toSKKData :: FilePath -> IO [SKK]
toSKKData fp = catMaybes <$> P.map lineToSKKData <$> readUTF8line fp

getAddDictR :: Text -> Handler ()
getAddDictR _ = do
  runDB $ do
    runMigration migrateAll
    skks <- liftIO $ toSKKData "d:/home/.skk-jisyo.utf8"
    forM_ (zip [0..] skks) $ \(n, skk) -> do
      _ <- insert skk
      return ()

postAddDictR :: Text -> Handler Html
postAddDictR dictName = error "Not yet implemented: postAddDictR"

