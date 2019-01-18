{-# LANGUAGE AllowAmbiguousTypes #-}
module Match.SQL
  (fetchSQLSource
  , SQLSource (..)
  , initializeSQLS
  , initializeS
  , initializeL) where

import           Control.Arrow              ((>>>))
import           Control.Lens               hiding (Getter)
import           Control.Monad.Reader
import           Control.Exception.Safe
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Text                  (Text)
import qualified Data.Text                  as Tx
import           Database.SQLite.Simple
import           Util.Exception             (FileNotExistException (..))
import           Match.Config               ( PathGetter
                                            , Getter
                                            , readConf)
import           Match.CSV                  (parseCSV, Spec)
import           System.Directory           ( doesFileExist
                                            , getModificationTime
                                            , removeFile)

type ReadCSV = Either String [[Text]]

data TestField = TestField Int Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

fromString :: TestField -> Text
fromString (TestField _ a) = a

writeSQLite :: ReadCSV -> String -> IO ()
writeSQLite csv dbname = do
  conn <- open dbname
  case csv of
    Left _  -> return ()
    Right c -> do
      execute_ conn "CREATE TABLE test (id INTEGER PRIMARY KEY, str TEXT)"
      execute_ conn "BEGIN TRANSACTION"
      forM_ c $ \row ->
        execute conn "INSERT INTO test (str) VALUES (?)"
          (Only (Tx.intercalate "," row :: Text))
      execute_ conn "COMMIT TRANSACTION"
      close conn

readSQLite :: (MonadThrow m, MonadIO m) => String -> m [[Text]]
readSQLite dbname = do
  p <- liftIO $ doesFileExist dbname
  if p
    then do conn <- liftIO $ open dbname
            r    <- liftIO (query_ conn "SELECT * from test" :: IO [TestField])
            liftIO $ close conn
            return $ map (Tx.splitOn "," . fromString) r
    else throwM $ FileNotExistException dbname

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

csvToSQL :: Spec -> FilePath -> FilePath -> IO ()
csvToSQL sp csv db = do
  c <- sp `parseCSV` csv
  c `writeSQLite` db

readSQLiteSource :: (MonadThrow m, MonadIO m) => String -> Source m [Text]
readSQLiteSource dbname = do
  p <- liftIO $ doesFileExist dbname
  if p
    then do conn <- liftIO $ open dbname
            r    <- liftIO (query_ conn "SELECT * from test" :: IO [TestField])
            liftIO $ close conn
            mapM_ (fromString >>> Tx.splitOn "," >>> yield) r
    else throwM $ FileNotExistException dbname

fetchSQLSource :: (MonadThrow m, MonadIO m) =>
  PathGetter -> Spec -> PathGetter -> Source m [Text]
fetchSQLSource csvf spec dbf = do
  conf <- lift readConf
  let csv = conf ^. csvf
  let db  = conf ^. dbf

  liftIO $ renewDB spec csv db

  readSQLiteSource db

fetchSQLSource2 :: (MonadThrow m, MonadIO m) =>
  PathGetter -> Getter [Text] -> PathGetter -> Source m [Text]
fetchSQLSource2 csvf specf dbf = do
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

initializeSQLS :: SQLSource a -> Source IO [Text]
initializeSQLS sql =
  fetchSQLSource2 (csvPathGetter sql) (specGetter sql) (dbPathGetter sql)

initializeS :: SQLSource a -> Source IO a
initializeS sql = (initializeSQLS sql) $= CL.map (makeFunction sql)

initializeL :: SQLSource a -> IO [a]
initializeL sql = runConduit $ initializeS sql .| CL.consume
