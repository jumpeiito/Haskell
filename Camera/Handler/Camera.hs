 {-# OverloadedStrings #-} 
module Handler.Camera where

import Data.Time.Clock
import Data.List ((!!))
import Import
import Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))
import System.Directory
import System.Process
import qualified System.IO as I
import qualified Data.Text.IO as Tx
import Util

excel, sqlite :: FileSystem
excel  = File [ "c:/Users/Jumpei/Haskell/組合員名簿.xlsm"
              , "s:/馬場フォルダ/組合員名簿/組合員名簿.xlsm"]
sqlite = File [ "c:/Users/Jumpei/Haskell/Camera/Camera.sqlite3"
              , "d:/home/Haskell/Camera/Camera.sqlite3"]

removeFileIfExists :: Maybe FilePath -> IO ()
removeFileIfExists Nothing = return ()
removeFileIfExists (Just path) = removeFile path

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
  gen      <- liftIO $ meiboMain "全"
  runDB $ do
    runMigration migrateAll
    let meibo = zip [0..] gen
    forM_ meibo $ \(n, line) -> do
      let han'  = han line
      let bk    = bunkai line
      let name' = Meibo.Base.name line
      let ad'   = addressStr line
      let tel'  = telephoneStr line
      _ <- insert $ Person n han' bk name' ad' tel'
      return ()

deleteDB :: HandlerT App IO ()
deleteDB = runDB $ deleteWhere ([] :: [Filter Person])

refreshDB :: String -> HandlerT App IO ()
refreshDB bunkai = do
  timing <- liftIO timingP
  case timing of
    False -> return ()
    True  -> deleteDB >> insertDB

getBunkai :: String -> HandlerT App IO [Person]
getBunkai bk = runDB $ do
  bkn <- selectList [PersonBunkai ==. bk] []
  return $ map entityVal bkn

bunkaiHrefWidget :: WidgetT App IO ()
bunkaiHrefWidget = do
  let list = ["全", "石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]
  toWidget [whamlet|
                   $forall bun <- list
                      <a href=@{CameraR bun}>#{bun}
                   |]

getCameraR :: String -> Handler Html
getCameraR bunkai = do
  (_, sout, _, _) <- liftIO $ runInteractiveProcess "ls" ["-shal", "d:/home/Haskell/Makefile"] Nothing Nothing
  hoge <- liftIO $ lines <$> Tx.hGetContents sout

  liftIO $ mapM_ putStrLn hoge

  refreshDB bunkai

  ex <- liftIO $ getModificationTime =<< fromMaybe "" <$> runFile excel
  sq <- liftIO $ getModificationTime =<< fromMaybe "" <$> runFile sqlite

  meibo <- getBunkai bunkai
  let persons = zip [0..] meibo :: [(Int, Person)]
  defaultLayout $ do
    bunkaiHrefWidget
    addScript $ StaticR js_Camera_js
    $(widgetFile "camera")

postCameraR :: String -> Handler Html
postCameraR bunkai = error "Not yet implemented: getCameraR"
