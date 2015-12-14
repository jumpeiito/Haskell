module Util where

import Data.List
import Data.Time
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import System.Directory
import qualified Data.Map as Map
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, hClose, mkTextEncoding, stdout, utf8, hPutStrLn, Handle)
import qualified Data.ByteString.Lazy as B
import Text.ParserCombinators.Parsec

split :: Char -> String -> [String]
split sep str =
  loop str [] []
  where (&) a b = reverse b:a
        loop "" big small = reverse (big&small)
        loop (x:xs) big small | x == sep  = loop xs (big&small) []
                              | otherwise = loop xs big (x:small)

connect :: String -> [String] -> String
connect sep []     = ""
connect sep [x]    = x
connect sep (x:xs) = x++sep++connect sep xs

string2Date :: String -> Day
string2Date date =
  fromGregorian y m d
  where y':m:d:_ = map (\m -> read m :: Int) $ split '/' date
        y = toInteger y'

readUTF8File :: FilePath -> IO String
readUTF8File fp = do
  h <- openFile fp ReadMode
  encoding <- mkTextEncoding "cp65001"
  hSetEncoding h encoding
  hGetContents h

readUTF8ByteFile :: FilePath -> IO B.ByteString
readUTF8ByteFile fp = do
  h <- openFile fp ReadMode
  encoding <- mkTextEncoding "cp65001"
  hSetEncoding h encoding
  B.hGetContents h

withOutFile :: FilePath -> (Handle -> IO ()) -> IO ()
withOutFile oFile func = do
  h <- openFile oFile WriteMode
  encoding <- mkTextEncoding "cp65001"
  hSetEncoding h encoding
  func h
  hClose h

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File fp contents = do
  h <- (openFile fp WriteMode)
  encoding <- mkTextEncoding "cp65001"
  hSetEncoding h encoding
  hPutStrLn h contents
  hClose h

(</>) :: FilePath -> String -> FilePath
(</>) dirname filename =
  if "/" `isSuffixOf` dirname
  then dirname ++ filename
  else dirname ++ "/" ++ filename

-- forM :: Monad m => [a] -> (a -> m b) -> m [b]
-- forM a f = mapM f a

-- getRecursive :: FilePath -> IO [FilePath]
-- getRecursive fp = do
--   paths <- filter (\n -> notElem n [".", ".."]) $ getDirectoryContents fp
--   let datum = map ((</>) fp) paths
--   forM datum $ \hp -> do
--     existance <- doesDirectoryExist hp
--     if existance
--       then getRecursive hp
--       else return [hp]
--   return $ concat datum
