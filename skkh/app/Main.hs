{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text           as Tx
import           Network             (listenOn
                                     , withSocketsDo
                                     , accept
                                     , PortID(..)
                                     , Socket)
import           System.Environment  (getArgs)
import           System.IO           (hSetBuffering
                                     , hGetLine
                                     , hPutStrLn
                                     , BufferMode(..)
                                     , Handle)
import qualified System.IO           as I
import           System.Directory    (doesFileExist)
import           Control.Concurrent  (forkIO)
import           Text.Parsec
import           Util

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SKK
    query Text
    candidate Text
    deriving Show
|]

data Command = Lookup String
             | Version
  deriving Show

shrink :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
shrink = id

dbname = "d:/home/temp/Haskell/skkh/skk.sqlite3"
skkjisyo = "d:/home/.skk-jisyo"

withDB :: (FilePath -> IO a) -> IO a
withDB f = f dbname

withSqlite f = do
  runSqlite (Tx.pack dbname) . shrink $ f

makeNewDB :: IO ()
makeNewDB = do
  withDB $ \fp -> do
    p <- doesFileExist fp
    when (not p) $
      runSqlite (Tx.pack fp) $ runMigration migrateAll

insertDB :: SKK -> IO ()
insertDB skk = withSqlite $ do
  insert skk >> return ()

lineToSKKData :: Text -> Maybe SKK
lineToSKKData skkdictline =
  case Tx.split (==' ') skkdictline of
    [cd, q] -> Just $ SKK cd q
    _       -> Nothing

toSKKData :: FilePath -> IO [SKK]
toSKKData fp = catMaybes <$> map lineToSKKData <$> readUTF8line fp

insertDBfromSKK :: IO ()
insertDBfromSKK = do
  withSqlite $ do
    runMigration migrateAll
    skks <- liftIO $ toSKKData "d:/home/.skk-jisyo.utf8"
    forM_ skks insert

lookupDB :: Text -> IO (Maybe SKK)
lookupDB key = do
  query <- withSqlite $ do
    map entityVal <$> selectList [SKKQuery ==. key] []
  case query of
    []  -> return Nothing
    h:_ -> return $ Just h

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

parseCommand :: String -> Maybe Command
parseCommand ('1':k) = Just (Lookup $ takeWhile (/= ' ') k)
parseCommand ('2':k) = Just Version
parseCommand _ = Nothing

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (h, _, _) <- accept sock

    hSetBuffering h NoBuffering
    forkIO $ commandProcessor h
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor h = do
    I.hSetEncoding I.stdin I.utf8
    I.hSetEncoding I.stdout I.utf8
    I.hSetEncoding h I.utf8
    line <- hGetLine h
    putStrLn line
    -- let cmd = words line
    -- case (head cmd) of
    --     ("echo") -> echoCommand h cmd
    --     ("add") -> addCommand h cmd
    --     ("rev") -> revCommand h cmd
    --     ("fib") -> fibCommand h cmd
    --     ("fact") -> factCommand h cmd
    --     _ -> do hPutStrLn h "Unknown command"
    case parseCommand line of
      Just Version    -> hPutStrLn h "version 0.0.0"
      Just (Lookup k) -> do
        val <- lookupDB (Tx.pack $ init k)
        case val of
          Just l  -> hPutStrLn h (Tx.unpack (sKKCandidate l))
          Nothing -> do
            hPutStrLn h k
            hPutStrLn h "Not found"
      _ -> return ()
    commandProcessor h

-- echoCommand :: Handle -> [String] -> IO ()
-- echoCommand h cmd = do
--     hPutStrLn h (unwords $ tail cmd)

-- addCommand :: Handle -> [String] -> IO ()
-- addCommand h cmd = do
--     hPutStrLn h $ show $ (read $ cmd !! 1) + (read $ cmd !! 2)

-- revCommand :: Handle -> [String] -> IO()
-- revCommand h cmd = do
--     hPutStrLn h (reverse $ unwords $ tail cmd)

-- fibCommand :: Handle -> [String] -> IO()
-- fibCommand h cmd = do
--     hPutStrLn h (show $ fib $ read $ cmd !! 1)

-- fib :: Integer -> Integer
-- fib 0 = 1
-- fib 1 = 1
-- fib n = fib (n-2) + fib (n- 1)

-- factCommand :: Handle -> [String] -> IO()
-- factCommand h cmd = do
--     hPutStrLn h (show $ fact $ read $ cmd !! 1)

-- fact :: Integer -> Integer
-- fact 0 = 1
-- fact n = n * fact (n - 1)
