module Main where

import Util
import Control.Monad.Reader
import System.Process

data Config = Con { haskellDir :: FilePath
                  , direction  :: FileDirect
                  , hasktags   :: FilePath
                  , options    :: [String]
                  }

config :: Config
config = Con { haskellDir = "C:/Users/jumpei/Haskell/"
             , direction  = FD (".stack-work" <!~>) ("hs" <^>)
             , hasktags   = "C:/Users/Jumpei/AppData/Roaming/local/bin/hasktags.exe"
             , options    = ["-o", "C:/Users/jumpei/Haskell/TAGS", "-e"]}

collectFiles :: ReaderT Config IO [FilePath]
collectFiles = do
  directory <- haskellDir <$> ask
  direct    <- direction  <$> ask
  files     <- liftIO $ allfd directory direct
  return files

makeTags :: [FilePath] -> ReaderT Config IO ()
makeTags files = do
  command <- hasktags <$> ask
  option  <- options  <$> ask
  liftIO $ runInteractiveProcess command (option ++ files) Nothing Nothing
  return ()

main :: IO ()
main = do
  files <- collectFiles `runReaderT` config
  forM_ files putStrLn
  makeTags files `runReaderT` config
