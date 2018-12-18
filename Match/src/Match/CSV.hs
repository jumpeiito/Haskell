-- -*- coding:utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
module Match.CSV
  (parseCSV, parseCSV2, Spec, parseCSVSource)
where

import           Control.Arrow         ((>>>))
import           Control.Exception.Safe
import           Control.Monad.Trans
import           Data.Conduit
import           Data.List
import           Data.Text              (Text, replace)
import           Util.Exception         (CSVParseFailException (..)
                                        , FileNotExistException (..))
import           System.Directory       (doesFileExist)
import qualified Text.ParseCSV          as CSV
import           Util

type Spec      = [Text]
type SpecIndex = [Maybe Int]
type Record    = [Text]

-- |
--
-- >>> :set -XOverloadedStrings
--
-- >>> [Just 0, Just 2, Nothing] `extractColumns` ["hoge", "foo", "buz"]
-- ["hoge","buz"]
-- >>> [Nothing, Nothing] `extractColumns` ["hoge", "foo", "buz"]
-- []
extractColumns :: SpecIndex -> Record -> Record
extractColumns spec record' = foldr extract [] spec
  where
    size = length record'
    extract Nothing seed = seed
    extract (Just s) seed
      | size > s = record' !! s : seed
      | otherwise = seed

commaReplace :: Record -> Record
commaReplace = map $ replace "," "・"

-- |
--
-- >>> parseHeader ["hoge", "foo"] ["buz", "buz", "hoge", "buz", "foo"]
-- [Just 2,Just 4]
parseHeader :: Spec -> Record -> SpecIndex
parseHeader spec header = map (`elemIndex` header) spec

parseCSV :: Spec -> FilePath -> IO (Either String CSV.CSV)
parseCSV spec fp = do
  contents <- readSJIS fp :: IO Text
  case CSV.parseCSV contents of
    Left pe -> return $ Left pe
    Right c -> let sindex = spec `parseHeader` head c
               in return $ Right $ map (commaReplace . (sindex `extractColumns`)) $ tail c

parseCSV2 :: (MonadThrow m, MonadIO m)
  => Spec -> FilePath -> m [[Text]]
parseCSV2 spec fp = do
  p <- liftIO $ doesFileExist fp
  if p
    then do contents <- liftIO $ readSJIS fp
            case CSV.parseCSV contents of
              Left _  -> throwM $ CSVParseFailException fp
              Right c -> do
                let sindex = spec `parseHeader` head c
                return $ map (sindex `extractColumns`) $ tail c
    else throwM $ FileNotExistException fp

parseCSVSource :: (MonadThrow m, MonadIO m)
  => Spec -> FilePath -> Source m [Text]
parseCSVSource spec fp = do
  p <- liftIO $ doesFileExist fp
  if p
    then do contents <- liftIO $ readSJIS fp
            case CSV.parseCSV contents of
              Left _  -> throwM $ CSVParseFailException fp
              Right c -> do
                let yield' = commaReplace >>> yield
                case spec `parseHeader` head c of
                  []     -> mapM_ yield' $ tail c
                  sindex -> mapM_ ((sindex `extractColumns`) >>> yield') $ tail c
    else throwM $ FileNotExistException fp

