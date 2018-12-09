{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Match.Config
  ( PathGetter
  , Config (..)
  , readConfig
  , sendCSVFileName
  , fileTreeDirectory
  , directorySpecF
  , hihoSpecF
  , kumiaiSpecF
  , officeSpecF
  , kumiaiOfficeSpecF)
where

import           Control.Exception.Safe
import           Control.Monad.Trans
-- import           Control.Monad.Trans.Maybe
import           Data.Aeson
-- import qualified Data.ByteString.Char8     as BS
import           Data.Text                 (Text)
import           GHC.Generics
-- import           System.Directory          (doesFileExist)
-- import           Util.Exception           ( FileNotExistException (..)
--                                           , YamlParseFailException (..))
import           Util.Yaml                (readYaml)

data Config = Config { directoryFile    :: ! String
                     , directorySpec    :: [Text]
                     , hihoSpec         :: [Text]
                     , kumiaiSpec       :: [Text]
                     , officeSpec       :: [Text]
                     , kumiaiOfficeSpec :: [Text]
                     , fileTree         :: ! String
                     , kumiaiFile       :: ! String
                     , officeFile       :: ! String
                     , kumiaiOfficeFile :: ! String
                     , hihoFile         :: ! String
                     , kumiaiDB         :: ! String
                     , officeDB         :: ! String
                     , kumiaiOfficeDB   :: ! String
                     , hihoDB           :: ! String }
  deriving (Show, Generic)

instance FromJSON Config

type PathGetter = Config -> FilePath

readConfig :: (MonadThrow m, MonadIO m) => m Config
readConfig = readYaml "d:/home/matchConfig.yaml"

safeReadConfig :: (MonadCatch m, MonadIO m) => m Config
safeReadConfig =
  readConfig `catch`
    (\(SomeException a) -> do liftIO $ print a
                              return defaultConfig)

sendCSVFileName :: (MonadCatch m, MonadIO m) => m FilePath
sendCSVFileName = directoryFile <$> safeReadConfig

fileTreeDirectory :: (MonadCatch m, MonadIO m) => m FilePath
fileTreeDirectory = fileTree <$> safeReadConfig

directorySpecF, hihoSpecF, kumiaiSpecF, officeSpecF, kumiaiOfficeSpecF
  :: (MonadCatch m, MonadIO m) => m [Text]
directorySpecF    = directorySpec    <$> safeReadConfig
hihoSpecF         = hihoSpec         <$> safeReadConfig
kumiaiSpecF       = kumiaiSpec       <$> safeReadConfig
officeSpecF       = officeSpec       <$> safeReadConfig
kumiaiOfficeSpecF = kumiaiOfficeSpec <$> safeReadConfig

defaultConfig :: Config
defaultConfig =
  Config { directoryFile    = "d:/送信案件一覧.csv"
         , directorySpec    = []
         , hihoSpec         = []
         , kumiaiSpec       = []
         , officeSpec       = []
         , kumiaiOfficeSpec = []
         , fileTree         = "y:/労働保険事務組合/電子申請e-Gov"
         , kumiaiFile       = ""
         , officeFile       = ""
         , kumiaiOfficeFile = ""
         , hihoFile         = ""
         , kumiaiDB         = ""
         , officeDB         = ""
         , kumiaiOfficeDB   = ""
         , hihoDB           = "" }
