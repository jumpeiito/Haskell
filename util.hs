{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Util where

import Data.List
-- import Data.Time
-- import Debug.Trace
-- import Control.Applicative              hiding ((<|>), many)
import Control.Exception                hiding (try)
import Control.Monad
import Control.Monad.Writer
import System.Directory
import qualified Data.Map               as Map
import qualified Control.Monad.State    as St
import qualified System.IO              as I
import qualified Data.Text              as Tx
import qualified Data.Text.IO           as Txio
import qualified Data.Text.Internal     as Txi
import qualified Data.ByteString.Char8  as B
import qualified Text.StringLike        as Like
import Text.Parsec
import Text.Parsec.String
-- import Text.ParserCombinators.Parsec    hiding (try, Parser)

class Splittable a where
  split :: Char -> a -> [a]

instance Splittable String where
  split sep str = reverse . fst . (`St.execState` (mempty, mempty)) $ do
    forM_ str $ \ch -> do
      (big, small) <- St.get
      if ch == sep
        then St.put (reverse small:big, [])
        else St.put (big, ch : small)
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
  joiner _ [] = ""
  joiner _ [x] = x
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

alld' :: FilePath -> IO [FilePath]
alld' dir = do
  let cut = map ((dir <>) . ("/" <>)) . filter (`notElem` [".", ".."])
  contents <- cut <$> getDirectoryContents dir
  filterM doesDirectoryExist contents

alld :: FilePath -> IO [FilePath]
alld dir = alld' dir >>= mapM alld' >>= return . concat

makeMap :: Ord k => (t -> k) -> (t -> a) -> [t] -> Map.Map k [a]
makeMap _ _ [] = Map.empty
makeMap kF vF (x:xs) =
  Map.insertWith' (++) (kF x) [vF x] $ makeMap kF vF xs

makeCountMap :: (Num a, Ord k) => (t -> k) -> [t] -> Map.Map k a
makeCountMap _ [] = Map.empty
makeCountMap kF (x:xs) =
   Map.insertWith' (+) (kF x) 1 $ makeCountMap kF xs

class ReadFile a where
  readUTF8     :: FilePath -> IO a
  readUTF8line :: FilePath -> IO [a]
  readSJIS     :: FilePath -> IO a
  readSJISline :: FilePath -> IO [a]

baseReadFile :: Like.StringLike a => String -> (I.Handle -> IO a) -> FilePath -> IO a
baseReadFile coding f fp =
  bracket (I.openFile fp I.ReadMode)
          (I.hClose)
          (\h -> do
              encoding <- I.mkTextEncoding coding
              I.hSetEncoding h encoding
              f h >>= evaluate)

baseReadUTF8 :: Like.StringLike a => (I.Handle -> IO a) -> FilePath -> IO a
baseReadUTF8 = baseReadFile "cp65001"

baseReadSJIS :: Like.StringLike a => (I.Handle -> IO a) -> FilePath -> IO a
baseReadSJIS = baseReadFile "cp932"

instance ReadFile String where
  readUTF8        = baseReadUTF8 I.hGetContents
  readUTF8line fp = lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS I.hGetContents
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
  h <- I.openFile fp I.ReadMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hGetContents h

readUTF8ByteFile :: FilePath -> IO B.ByteString
readUTF8ByteFile fp = do
  h <- I.openFile fp I.ReadMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  B.hGetContents h

withOutFile :: FilePath -> (I.Handle -> IO ()) -> IO ()
withOutFile oFile func = do
  h <- I.openFile oFile I.WriteMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  func h
  I.hClose h

withAppendFile :: FilePath -> (I.Handle -> IO ()) -> IO ()
withAppendFile oFile func = do
  h <- I.openFile oFile I.WriteMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  func h
  I.hClose h

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File fp contents = do
  h <- (I.openFile fp I.WriteMode)
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hPutStrLn h contents
  I.hClose h

appendUTF8File :: FilePath -> String -> IO ()
appendUTF8File fp contents = do
  h <- (I.openFile fp I.AppendMode)
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hPutStrLn h contents
  I.hClose h

(</>) :: FilePath -> String -> FilePath
(</>) dirname filename =
  if "/" `isSuffixOf` dirname
  then dirname ++ filename
  else dirname ++ "/" ++ filename

(&&&), (|||) :: Monad m => m Bool -> m Bool -> m Bool
(&&&) x y = do
  f1 <- x
  f2 <- y
  return $ f1 && f2

(|||) x y = do
  f1 <- x
  f2 <- y
  return $ f1 || f2

_include :: [String] -> Parser String
_include xs = do
  try $ choice $ map (try . string) xs
  <|> (anyChar >> _include xs)

include :: [String] -> String -> Bool
include xs target = either (const False) (const True)
                           $ parse (_include xs) "" target
