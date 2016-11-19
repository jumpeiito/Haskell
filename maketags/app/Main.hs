module Main where

import Util
import Control.Monad.Reader
import Data.Maybe
import System.Process

data Config = Con { topDir     :: FileSystem
                  , direction  :: FileDirect
                  }

type CfgReaderT a = ReaderT Config IO a

config :: Config
config = Con { topDir     = Directory ["C:/Users/jumpei/", "D:/home/"]
             , direction  = FD (".stack-work" <!~>) ("hs" <^>)
             }

topDirectory :: CfgReaderT FilePath
topDirectory = do
  maybeDir <- topDir <$> ask
  Just directory <- liftIO . runFile $ maybeDir
  return directory

optionList :: CfgReaderT [String]
optionList = do
  dir <- topDirectory
  return ["-o", dir ++ "TAGS", "-e"]

hasktags2 :: CfgReaderT FilePath
hasktags2 = do
  dir <- topDirectory
  return $ dir ++ "AppData/Roaming/local/bin/hasktags.exe"

collectFiles :: CfgReaderT [FilePath]
collectFiles = do
  directory <- topDirectory
  direct    <- direction  <$> ask
  files     <- liftIO $ allfd directory direct
  return files

makeTags :: [FilePath] -> CfgReaderT ()
makeTags files = do
  command <- hasktags2
  option  <- optionList
  liftIO $ runInteractiveProcess command (option ++ files) Nothing Nothing
  return ()

main :: IO ()
main = do
  files <- collectFiles `runReaderT` config
  forM_ files putStrLn
  makeTags files `runReaderT` config
