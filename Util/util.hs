{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Util where

import Data.List
import Control.Exception                hiding (try)
import Control.Monad
import Control.Monad.Trans              (liftIO)
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
----------------------------------------------------------------------------------------------------
data FileDirect =
  FD { dirP  :: FilePath -> Bool,
       fileP :: FilePath -> Bool }

(<~>), (<^>), (<!~>), (<!^>) :: FilePath -> FilePath -> Bool
(<~>) = isInfixOf
(<^>) = isSuffixOf
(<!~>) a b = not $ isInfixOf a b
(<!^>) a b = not $ isSuffixOf a b

allf :: FilePath -> IO [FilePath]
allf fp = allfd fp (FD (const True) (const True))

allfd :: FilePath -> FileDirect -> IO [FilePath]
allfd fp fd = snd <$> runWriterT (_allfd fp fd)

_allfd :: FilePath -> FileDirect -> WriterT [FilePath] IO ()
_allfd fp fd = _all_base fp fd f
  where f path bool = do
          case (bool, dirP fd fp, fileP fd path) of
            (True, _, _)        -> do { r <- liftIO $ allfd path fd; tell r }
            (False, True, True) -> tell [path]
            (False, _, _)       -> tell mempty

_all_base :: FilePath ->
             FileDirect ->
             (FilePath -> Bool -> WriterT [FilePath] IO b) ->
             WriterT [FilePath] IO ()
_all_base fp fd f = do
  let filtering = filter (`notElem` [".", ".."])
  let makePath  = map (\n -> fp ++ "/" ++ n)
  let cut       = makePath . filtering
  paths <- liftIO $ cut <$> getDirectoryContents fp
  forM_ paths $ \path -> do
    bool <- liftIO $ doesDirectoryExist path
    f path bool
----------------------------------------------------------------------------------------------------
_alld :: FilePath -> WriterT [FilePath] IO ()
_alld fp = _all_base fp fdn f
  where fdn = FD (const True) (const True)
        f path bool = do
          when bool $ do
            descend <- liftIO $ alld path
            tell $ [path] ++ descend

alld :: FilePath -> IO [FilePath]
alld fp = snd <$> runWriterT (_alld fp)
----------------------------------------------------------------------------------------------------
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

baseReadFile :: String -> (I.Handle -> IO a) -> FilePath -> IO a
baseReadFile coding f fp =
  bracket (I.openFile fp I.ReadMode)
          (I.hClose)
          (\h -> do
              encoding <- I.mkTextEncoding coding
              I.hSetEncoding h encoding
              f h >>= evaluate)

baseReadUTF8 :: (I.Handle -> IO a) -> FilePath -> IO a
baseReadUTF8 = baseReadFile "cp65001"

baseReadSJIS :: (I.Handle -> IO a) -> FilePath -> IO a
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
(&&&) x y = (&&) <$> x <*> y
(|||) x y = (||) <$> x <*> y

_include :: [String] -> Parser String
_include xs = do
  try $ choice $ map (try . string) xs
  <|> (anyChar >> _include xs)

include :: [String] -> String -> Bool
include xs target = either (const False) (const True)
                           $ parse (_include xs) "" target

(++++) :: Parser String -> Parser String -> Parser String
(++++) p1 p2 = (++) <$> p1 <*> p2

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n x = (drop n x) ++ (take n x)
