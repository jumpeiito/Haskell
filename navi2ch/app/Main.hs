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

data Config = Config { searchPaths     :: [FilePath]
                     , searchDirection :: FileDirect
                     , topPath         :: FilePath }

type ConfigReader a = Reader Config a
type ConfigReaderT a = ReaderT Config IO a

config :: Config
config = Config { searchPaths     = [ "C:/Users/Jumpei/.navi2ch/"
                                    , "C:/users/sibuc526.newnet/home/.navi2ch/"
                                    , "f:/home/.navi2ch/"]
                , searchDirection = FD ("info" <!~>) (".dat" <^>)
                , topPath         = "f:/.navi2ch" }
----------------------------------------------------------------------------------------------------
mkdirIfNotExists :: FilePath -> IO ()
mkdirIfNotExists fp = do
  p <- doesDirectoryExist fp
  unless p $ createDirectory fp

navi :: FilePath -> NaviFile
navi fp = case getFileSimplePath fp of
  Right sp -> N fp sp
  Left _   -> NError fp

newDir, newFile :: NaviFile -> ConfigReader FilePath
newDir (N _ (d, base)) = (<> "/" <> d <> "/") <$> topPath <$> ask
newDir (NError _)      = return ""

newFile nv@(N _ (_, base)) = (++ base) <$> newDir nv
newFile (NError _) = return ""

getFileSimplePathParser :: Parser SimplePath
getFileSimplePathParser = do
  let tailTake = reverse . take 2 . reverse
  [dir, file] <- tailTake <$> sepBy (many (noneOf "/")) (string "/")
  return (dir, file)

getFileSimplePath :: FilePath -> Either ParseError SimplePath
getFileSimplePath = parse getFileSimplePathParser ""

getFiles :: FileDirect -> FilePath -> IO [FilePath]
getFiles direction fp = do
  putStrLn $ fp ++ " Entering."
  exist <- doesDirectoryExist fp
  if exist
    then allfd fp direction
    else return []

getFile2 :: ConfigReaderT [FilePath]
getFile2 = do
  dirs   <- searchPaths <$> ask
  direct <- searchDirection <$> ask
  liftIO $ concat <$> mapM (getFiles direct) dirs

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
    if copiable
      then do { mkdirIfNotExists newd;
                print file;
                copyFile file newf }
      else putStr "."
