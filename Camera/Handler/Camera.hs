module Handler.Camera where

import Import
import           Util
import           Data.Time.Clock
import           Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import           System.Directory (removeFile, doesFileExist, getModificationTime)
-- import           Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite


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
  Just excelFile <- runFile excel
  sqlFile <- runFile sqlite
  exist <- doesFileExist excelFile
  case (sqlFile, exist) of
    (Just sql, True) -> do
      excelDate <- getModificationTime excelFile
      sqlDate <- getModificationTime sql
      return $ diffUTCTime excelDate sqlDate > 0
    (_, _) -> return True

insertDB = do
  gen <- liftIO $ meiboMain "全"
  runDB $ do
    let meibo = zip [0..] gen
    forM_ meibo $ \(n, line) -> do
      let bk    = bunkai line
      let name' = Meibo.Base.name line
      let ad'   = addressStr line
      let tel'  = telephoneStr line
      insert $ Person n bk name' ad' tel'
      return ()

refreshDB :: HandlerT App IO ()
refreshDB = do
  Just sql <- liftIO $ runFile sqlite

  bool <- liftIO timingP
  case bool of
    False -> return ()
    True  -> do
      liftIO $ removeFileIfExists sql
      insertDB
      return ()
  
getBunkai :: String -> HandlerT App IO [Person]
getBunkai bk = runDB $ do
  bkn <- selectList [PersonBunkai ==. bk] [LimitTo 200]
  return $ map entityVal bkn

getCameraR :: String -> Handler Html
getCameraR bunkai = do
  bool <- liftIO timingP
  -- refreshDB
  insertDB

  meibo <- getBunkai bunkai
  let persons = zip [0..] meibo :: [(Int, Person)]
  defaultLayout $ do
    $(widgetFile "camera")

postCameraR :: String -> Handler Html
postCameraR bunkai = error "Not yet implemented: postCameraR"
  
