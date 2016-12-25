{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}

-- module Handler.MakeMeibo (refreshDB, getBunkai) where
module Handler.MakeMeibo where

import           Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import qualified Meibo.Base as MB
import           Util
import           Data.Time.Clock
import           Import

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import           System.Directory (removeFile, doesFileExist, getModificationTime)
import           Control.Monad (when)
import           Database.Persist.TH
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    pid Int
    bunkai String
    name String
    address String
    tel String
    Primary pid
    deriving Show
|]

excel  = File [ "c:/Users/Jumpei/Haskell/組合員名簿.xlsm"
              , "s:/馬場フォルダ/組合員名簿/組合員名簿.xlsm"]
sqlite = "c:/SQlite/sample.db;"

timingP :: IO Bool
timingP = do
  Just excelFile <- runFile excel
  exist <- doesFileExist excelFile
  case exist of
    True -> do
      excelDate <- getModificationTime excelFile
      sqlDate <- getModificationTime sqlite
      return $ diffUTCTime excelDate sqlDate > 0
    False -> return True

insertDB :: IO ()
insertDB = do
  gen <- meiboMain "全"
  runSqlite "C:/SQlite/sample.db;" $ do
    runMigration migrateAll
    let meibo = zip [0..] gen
    forM_ meibo $ \(n, line) -> do
      let bk = bunkai line
      let name' = Meibo.Base.name line
      let ad' = addressStr line
      let tel' = telephoneStr line
      insert $ Person n bk name' ad' tel'
      return ()

refreshDB :: IO ()
refreshDB = do
  bool <- timingP
  if bool
    then insertDB
    else return ()

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

getBunkai :: String -> IO [Person]
getBunkai bk = runNoLoggingT $ runResourceT $ withSqliteConn sqlite $ runSqlConn $ do
  bkn <- selectList [PersonBunkai ==. bk] [LimitTo 200]
  return $ map entityVal bkn
  

getMakeMeiboR :: String -> HandlerT App IO Html
getMakeMeiboR bunkai = do
  liftIO refreshDB

  foo <- liftIO $ getBunkai bunkai

  let persons = zip [0..] foo :: [(Int, Person)]
  defaultLayout $ do
    bunkaiHrefWidget
    addScript $ StaticR js_MakeMeibo_js
    $(widgetFile "MakeMeibo")

