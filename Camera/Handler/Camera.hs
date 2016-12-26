module Handler.Camera where

import Import
import           Util
import           Data.Time.Clock
import           Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import           System.Directory (removeFile, doesFileExist, getModificationTime)

excel, sqlite :: FileSystem
excel  = File [ "c:/Users/Jumpei/Haskell/組合員名簿.xlsm"
              , "s:/馬場フォルダ/組合員名簿/組合員名簿.xlsm"]
sqlite = File [ "c:/Users/Jumpei/Haskell/Camera/Camera.sqlite3"
              , "d:/home/Haskell/Camera/Camera.sqlite3"]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

timingP :: IO Bool
timingP = do
  excelFile <- runFile excel
  sqlFile   <- runFile sqlite
  case (sqlFile, excelFile) of
    (Nothing, Just _) -> return True
    (Just sql', Just excel') -> do
      excelDate <- getModificationTime excel'
      sqlDate   <- getModificationTime sql'
      return $ diffUTCTime excelDate sqlDate > 0
    (_, _) -> return False

insertDB :: HandlerT App IO ()
insertDB = do
  gen <- liftIO $ meiboMain "全"
  runDB $ do
    let meibo = zip [0..] gen
    forM_ meibo $ \(n, line) -> do
      let bk    = bunkai line
      let name' = Meibo.Base.name line
      let ad'   = addressStr line
      let tel'  = telephoneStr line
      _ <- insert $ Person n bk name' ad' tel'
      return ()

refreshDB :: HandlerT App IO ()
refreshDB = do
  Just sql <- liftIO $ runFile sqlite

  timing <- liftIO timingP
  case timing of
    False -> return ()
    True  -> do
      liftIO $ removeFileIfExists sql
      runDB $ runMigration migrateAll
      insertDB
  
getBunkai :: String -> HandlerT App IO [Person]
getBunkai bk = runDB $ do
  bkn <- selectList [PersonBunkai ==. bk] []
  return $ map entityVal bkn

bunkaiList :: [String]
bunkaiList = ["全", "石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]

bunkaiHrefWidget :: WidgetT App IO ()
bunkaiHrefWidget = do
  toWidget [whamlet|
                   $forall bun <- bunkaiList
                      <a href=@{CameraR bun}>#{bun}
                   |]

getCameraR :: String -> Handler Html
getCameraR bunkai = do
  timing <- liftIO timingP

  refreshDB

  meibo <- getBunkai bunkai
  let persons = zip [0..] meibo :: [(Int, Person)]
  defaultLayout $ do
    bunkaiHrefWidget
    addScript $ StaticR js_Camera_js
    $(widgetFile "camera")

postCameraR :: String -> Handler Html
postCameraR bunkai = error "Not yet implemented: postCameraR"
  
