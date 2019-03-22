{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Match.SQL
  (fetchSQLSource
  , SQLSource (..)
  , Sourceable (..)) where

import           Control.Arrow           ((>>>))
import           Control.Lens            hiding (Getter)
import           Control.Monad.Reader
import           Control.Exception.Safe
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Text               (Text)
import qualified Data.Text               as Tx
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Util.Exception          (FileNotExistException (..))
import           Match.Config            ( PathGetter
                                         , Getter
                                         , readConf)
import           Match.CSV               (parseCSV, Spec)
import           System.Directory        ( doesFileExist
                                         , getModificationTime
                                         , removeFile)
import           Match.Base
import           Match.Hiho
import           Match.Office
import           Match.OfficeSP
import           Match.Kumiai
import           Match.KumiaiOffice
import           Match.Hitori

type ReadCSV = Either String [[Text]]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    num Int
    str Text
    deriving Show
|]

toText :: Person -> Text
toText (Person _ s) = s

writeSQLite :: ReadCSV -> FilePath -> IO ()
writeSQLite csv dbname = do
  runSqlite (Tx.pack dbname) $ do
    runMigration migrateAll

    case csv of
      Left _  -> return ()
      Right c ->
        forM_ (zip c [1..]) $ \(s, n) -> do
          insert (Person n (Tx.intercalate "," s))

csvToSQL :: Spec -> FilePath -> FilePath -> IO ()
csvToSQL sp csv db = do
  c <- sp `parseCSV` csv
  c `writeSQLite` db

renewDB :: Spec -> FilePath -> FilePath -> IO ()
renewDB spec csv db = do
  p <- doesFileExist db
  if p
    then do csvtime <- getModificationTime csv
            sqltime <- getModificationTime db
            when (csvtime > sqltime) $ do
              removeFile db
              csvToSQL spec csv db
    else csvToSQL spec csv db

readSQLiteSource :: (MonadThrow m, MonadIO m) => String -> Source m [Text]
readSQLiteSource dbname = do
  p <- liftIO $ doesFileExist dbname
  if p
    then do texts <- liftIO $ readSQLiteIfExists dbname
            mapM_ yield texts
    else throwM $ FileNotExistException dbname

shrink :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
shrink = id

readSQLiteIfExists :: FilePath -> IO [[Text]]
readSQLiteIfExists dbname = do
  runSqlite (Tx.pack dbname) . shrink $ do
    answer <- selectList [] []
    return $ map (entityVal >>> toText >>> Tx.splitOn ",") answer

fetchSQLSource :: (MonadThrow m, MonadIO m) =>
  PathGetter -> Getter [Text] -> PathGetter -> Source m [Text]
fetchSQLSource csvf specf dbf = do
  conf <- lift readConf
  let csv  = conf ^. csvf
  let db   = conf ^. dbf
  let spec = conf ^. specf

  liftIO $ renewDB spec csv db

  readSQLiteSource db

data SQLSource a =
  SQLSource { specGetter    :: Getter [Text]
            , csvPathGetter :: PathGetter
            , dbPathGetter  :: PathGetter
            , makeFunction  :: [Text] -> a }

class Sourceable a where
  source              :: SQLSource a
  initializeCSVSource :: Reader (SQLSource a) (Source IO [Text])
  initializeSource    :: Source IO a
  initializeList      :: IO [a]

  initializeCSVSource = do
    csv  <- csvPathGetter <$> ask
    db   <- dbPathGetter <$> ask
    spec <- specGetter <$> ask
    return $ fetchSQLSource csv spec db

  initializeSource = (`runReader` source) $ do
    maker <- makeFunction <$> ask
    src   <- initializeCSVSource
    return $ src $= CL.map maker

  initializeList = runConduit (initializeSource .| CL.consume)

instance Sourceable HihoR where
  source = SQLSource { specGetter    = #hihoSpec
                     , csvPathGetter = #hihoFile
                     , dbPathGetter  = #hihoDB
                     , makeFunction  = makeHiho }

instance Sourceable Office where
  source = SQLSource { specGetter    = #officeSpec
                     , csvPathGetter = #officeFile
                     , dbPathGetter  = #officeDB
                     , makeFunction  = makeOffice }

instance Sourceable OfficeSP where
  source = SQLSource { specGetter    = #officeSPSpec
                     , csvPathGetter = #officeSPFile
                     , dbPathGetter  = #officeSPDB
                     , makeFunction  = makeOfficeSP }

instance Sourceable Kumiai where
  source = SQLSource { specGetter    = #kumiaiSpec
                     , csvPathGetter = #kumiaiFile
                     , dbPathGetter  = #kumiaiDB
                     , makeFunction  = makeKumiai }

instance Sourceable KumiaiOffice where
  source = SQLSource { specGetter    = #kumiaiOfficeSpec
                     , csvPathGetter = #kumiaiOfficeFile
                     , dbPathGetter  = #kumiaiOfficeDB
                     , makeFunction  = makeKumiaiOffice }

instance Sourceable Hitori where
  source = SQLSource { specGetter    = #hitoriSpec
                     , csvPathGetter = #hitoriFile
                     , dbPathGetter  = #hitoriDB
                     , makeFunction  = makeHitori }
