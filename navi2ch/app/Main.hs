{-# LANGUAGE OverloadedStrings #-}

module Main where

import Util
import Control.Monad            (forM_)
import qualified Control.Exception as Ex
import Text.Parsec
import Text.Parsec.String
import qualified System.IO          as I

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

type SimplePath = (String, String)

getTitleParse :: Parser String
getTitleParse = head <$> sepBy (many $ noneOf "<>") (string "<>")

-- tags :: Parser String
-- tags = do
--   elm <- between (char '<') (char '>') (many1 $ noneOf ">")
--   return $ "" ++ elm ++ ""
-- except :: Parser String
-- except = do
--   exc <- choice [try tags, many1 $ noneOf "<>"]
--   aft <- except
--   return $ exc ++ aft

getFileSimplePathParser :: Parser SimplePath
getFileSimplePathParser = do
  let tailTake = reverse . take 2 . reverse
  [dir, file] <- tailTake <$> sepBy (many (noneOf "/")) (string "/")
  return (dir, file)

getTitle :: String -> Either ParseError String
getTitle s = reverse <$> parse getTitleParse "" (reverse s)

getFileSimplePath :: FilePath -> Either ParseError SimplePath
getFileSimplePath = parse getFileSimplePathParser ""

getFiles :: FilePath -> IO [FilePath]
getFiles fp = allfd fp (FD ("info" <!~>) (".dat" <^>))

main :: IO ()
main = do
  files <- getFiles "f:/haskell/dat"
  forM_ files $ \file -> do
    contents <- head <$> sjisLines file
    let title = either (const file) id (getTitle contents)
    -- sjis <- I.mkTextEncoding "CP932"
    I.hSetEncoding I.stdout I.utf8
    -- putStrLn title
    -- putStrLn title `Ex.catch` (\(Ex.SomeException e) -> putStrLn (Ex.displayException e))
    putStrLn title `Ex.catch` (\(Ex.SomeException e) -> putStrLn $ file ++ (Ex.displayException e))

