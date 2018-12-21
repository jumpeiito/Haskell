module Util.Yaml where

import           Control.Exception.Safe
import           Control.Monad.Trans
import           Util.Exception
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS
import qualified Data.Yaml                 as Y
import           System.Directory          (doesFileExist)

readYaml ::
  (FromJSON a) =>
  (MonadThrow m, MonadIO m) =>
  FilePath -> m a
readYaml yamlFile = do
  p <- liftIO $ doesFileExist yamlFile
  if p
    then do content <- liftIO $ BS.readFile yamlFile
            case (Y.decode content :: FromJSON a => Maybe a) of
              Nothing       -> throwM YamlParseFailException
              Just yamlData -> return yamlData
    else throwM $ FileNotExistException yamlFile
