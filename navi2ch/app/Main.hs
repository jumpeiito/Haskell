{-# LANGUAGE OverloadedStrings #-}

module Main where

import Util
import Control.Monad                    (forM_, when)
import Control.Monad.Reader
import Control.Exception                
import Data.Monoid
import Text.Parsec
import Text.Parsec.String
import System.Directory
import qualified System.IO              as I

type SimplePath = (String, String)

data NaviFile   =
  N FilePath SimplePath
  | NError FilePath
  deriving (Show)

data Config = Config { searchPaths :: [FilePath]
                     , topPath     :: FilePath }

config = Config { searchPaths = ["C:/Users/Jumpei/.navi2ch/"
                                , "C:/users/sibuc526.newnet/home/.navi2ch/"
                                , "f:/home/.navi2ch/"]
                , topPath     = "f:/.navi2ch" }
----------------------------------------------------------------------------------------------------
mkdirIfNotExists :: FilePath -> IO ()
mkdirIfNotExists fp = do
  p <- doesDirectoryExist fp
  unless p $ createDirectory fp

navi :: FilePath -> NaviFile
navi fp = case getFileSimplePath fp of
  Right sp -> N fp sp
  Left _   -> NError fp

newDir, newFile :: NaviFile -> Reader Config FilePath
newDir (N _ (d, base)) = do
  top <- topPath <$> ask
  return $ top <> "/" <> d <> "/"
newDir (NError _) = return ""

newFile nv@(N _ (_, base)) = (++ base) <$> newDir nv
newFile (NError _) = return ""

getFileSimplePathParser :: Parser SimplePath
getFileSimplePathParser = do
  let tailTake = reverse . take 2 . reverse
  [dir, file] <- tailTake <$> sepBy (many (noneOf "/")) (string "/")
  return (dir, file)

getFileSimplePath :: FilePath -> Either ParseError SimplePath
getFileSimplePath = parse getFileSimplePathParser ""

getFiles :: FilePath -> IO [FilePath]
getFiles fp = do
  exist <- doesDirectoryExist fp
  if exist
    then allfd fp (FD ("info" <!~>) (".dat" <^>))
    else return []

getFile2 :: ReaderT Config IO [FilePath]
getFile2 = do
  dirs  <- searchPaths <$> ask
  liftIO $ concat <$> mapM getFiles dirs

fileSize :: FilePath -> IO Integer
fileSize fp = I.withFile fp I.ReadMode I.hFileSize
              
existsOrNewer :: FilePath -> FilePath -> IO Bool
existsOrNewer fp newfp = do
  exists  <- doesFileExist newfp
  if exists
    then do { size    <- fileSize fp;
              newsize <- fileSize newfp;
              return $ size < newsize }
    else return True
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  files <- getFile2 `runReaderT` config
  forM_ files $ \file -> do
    let nv = navi file
    let newd = newDir nv  `runReader` config
    let newf = newFile nv `runReader` config
    copiable <- existsOrNewer file newf
    when copiable $ do
      mkdirIfNotExists newd
      print file
      copyFile file newf
