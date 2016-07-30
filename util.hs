{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Util where

import Data.List
import Data.Time
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Writer
import System.Directory
import qualified Data.Map as Map
import qualified Control.Monad.State as St
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, hClose, mkTextEncoding, stdout, utf8, hPutStrLn, Handle)
import qualified Data.Text as Tx
import qualified Data.Text.IO as Txio
import qualified Data.Text.Internal as Txi
import qualified Data.ByteString.Char8 as B
import qualified Text.StringLike as Like
import Text.ParserCombinators.Parsec

class Splittable a where
  split :: Char -> a -> [a]

instance Splittable String where
  split sep str = reverse . fst . (`St.execState` (mempty, mempty)) $ do
    forM_ str $ \char -> do
      (big, small) <- St.get
      if char == sep
        then St.put (reverse small:big, [])
        else St.put (big, char : small)
    (big, small) <- St.get
    St.put (reverse small:big, [])

instance Splittable B.ByteString where
  split sep bstr = map B.pack $ split sep $ B.unpack bstr

instance Splittable Txi.Text where
  split sep bstr = map Tx.pack $ split sep $ Tx.unpack bstr

uniq :: Eq a => [a] -> [a]
uniq s = reverse . (`St.execState` []) $ do
  St.forM_ s $ \n -> do
    r <- St.get
    if n `notElem` r
      then St.put (n:r)
      else St.put r
  return ()

class Like.StringLike a => Join a where
  joiner :: String -> [a] -> a

instance Join String where
  joiner glue [] = ""
  joiner glue [x] = x
  joiner glue (x:y:xs) = x <> glue <> y <> rest
    where rest = if null xs
                 then ""
                 else glue <> joiner glue xs

allf :: FilePath -> IO [FilePath]
allf dir = do
  let cut = map ((dir <>) . ("/" <>)) . filter (`notElem` [".", ".."])
  paths    <- cut <$> getDirectoryContents dir
  ans <- forM paths $ \p -> do
    bool <- doesDirectoryExist p
    if bool
      then allf p
      else return [p]
  return $ concat ans

alld :: FilePath -> IO [FilePath]
alld dir = do
  files <- allf dir
  filterM doesDirectoryExist files
  
  
makeMap :: Ord k => (t -> k) -> (t -> a) -> [t] -> Map.Map k [a]
makeMap kF vF [] = Map.empty
makeMap kF vF (x:xs) =
  Map.insertWith' (++) (kF x) [vF x] $ makeMap kF vF xs

makeCountMap :: (Num a, Ord k) => (t -> k) -> [t] -> Map.Map k a
makeCountMap kF [] = Map.empty
makeCountMap kF (x:xs) =
   Map.insertWith' (+) (kF x) 1 $ makeCountMap kF xs

class ReadFile a where
  readUTF8     :: FilePath -> IO a
  readUTF8line :: FilePath -> IO [a]
  readSJIS     :: FilePath -> IO a
  readSJISline :: FilePath -> IO [a]

baseReadFile :: Like.StringLike a => String -> (Handle -> IO a) -> FilePath -> IO a
baseReadFile coding f fp = do
    h <- openFile fp ReadMode
    encoding <- mkTextEncoding coding
    hSetEncoding h encoding
    r <- f h
    hClose h
    return r

baseReadUTF8 :: Like.StringLike a => (Handle -> IO a) -> FilePath -> IO a
baseReadUTF8 = baseReadFile "cp65001"

baseReadSJIS :: Like.StringLike a => (Handle -> IO a) -> FilePath -> IO a
baseReadSJIS = baseReadFile "cp932"

instance ReadFile String where
  readUTF8        = baseReadUTF8 hGetContents
  readUTF8line fp = lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS hGetContents
  readSJISline fp = lines <$> readSJIS fp

instance ReadFile B.ByteString where
  readUTF8        = baseReadUTF8 B.hGetContents
  readUTF8line fp = B.lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS B.hGetContents
  readSJISline fp = B.lines <$> readSJIS fp
    
instance ReadFile Txi.Text where
  readUTF8        = baseReadUTF8 Txio.hGetContents
  readUTF8line fp = Tx.lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS Txio.hGetContents
  readSJISline fp = Tx.lines <$> readSJIS fp

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

withAppendFile :: FilePath -> (Handle -> IO ()) -> IO ()
withAppendFile oFile func = do
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

appendUTF8File :: FilePath -> String -> IO ()
appendUTF8File fp contents = do
  h <- (openFile fp AppendMode)
  encoding <- mkTextEncoding "cp65001"
  hSetEncoding h encoding
  hPutStrLn h contents
  hClose h

(</>) :: FilePath -> String -> FilePath
(</>) dirname filename =
  if "/" `isSuffixOf` dirname
  then dirname ++ filename
  else dirname ++ "/" ++ filename
