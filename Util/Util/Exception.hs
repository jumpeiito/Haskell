module Util.Exception ( FileNotExistException (..)
                      , YamlParseFailException (..)
                      , CSVParseFailException (..)) where

import Control.Exception

data FileNotExistException  =
  FileNotExistException FilePath deriving Show
data CSVParseFailException  =
  CSVParseFailException FilePath deriving Show
data YamlParseFailException = YamlParseFailException deriving Show

instance Exception FileNotExistException
instance Exception CSVParseFailException
instance Exception YamlParseFailException
