{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.MakeMeibo where

import           Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import qualified Meibo.Base as MB
import           Import

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when)
import           Database.Persist.TH
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    pid Int
    name String
    deriving Show
|]


bunkaiList :: [String]
bunkaiList = ["全", "石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]

bunkaiHrefWidget :: WidgetT App IO ()
bunkaiHrefWidget = do
  toWidget [whamlet|
                   $forall bun <- bunkaiList
                      <a href=@{MakeMeiboR bun}>#{bun}
                   |]

concernP :: Line -> Bool
concernP = ("付" `isInfixOf`) . MB.exp

getMakeMeiboR :: String -> HandlerT App IO Html
getMakeMeiboR bunkai = do
  liftIO $ runSqlite "c:/SQLite/sample.db;" $ do
    -- this line added: that's it!
    runMigration migrateAll
    michaelId <- insert $ Person 1 "Michael"
    michael <- get michaelId
    liftIO $ print michael

  foo <- liftIO $ meiboMain bunkai

  let persons = zip [0..] foo :: [(Int, Line)]
  defaultLayout $ do
    bunkaiHrefWidget
    addScript $ StaticR js_MakeMeibo_js
    $(widgetFile "MakeMeibo")

