module Match.Config
  ( PathGetter
  , Conf (..)
  , readConf
  , sendCSVFileName
  , fileTreeDirectory
  , directorySpecF
  , hihoSpecF
  , kumiaiSpecF
  , officeSpecF
  , kumiaiOfficeSpecF)
where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Trans
import           Data.Extensible
import           Data.Text                (Text)
import           Data.Yaml
import           Util.Exception
import           System.Directory         (doesFileExist)

type Conf = Record
  '[ "directoryFile"    >: String
   , "directorySpec"    >: [Text]
   , "hihoSpec"         >: [Text]
   , "kumiaiSpec"       >: [Text]
   , "officeSpec"       >: [Text]
   , "kumiaiOfficeSpec" >: [Text]
   , "fileTree"         >: String
   , "kumiaiFile"       >: String
   , "officeFile"       >: String
   , "kumiaiOfficeFile" >: String
   , "hihoFile"         >: String
   , "kumiaiDB"         >: String
   , "officeDB"         >: String
   , "kumiaiOfficeDB"   >: String
   , "hihoDB"           >: String
   ]

type PathGetter = Conf -> FilePath

readConf :: (MonadThrow m, MonadIO m) => m Conf
readConf = do
  let file = "d:/home/matchConfig.yaml"
  p <- liftIO $ doesFileExist file
  if p
    then do yaml <- liftIO $ decodeFileEither file
            case yaml of
              Left _ -> throwM YamlParseFailException
              Right c -> return c
    else throwM $ FileNotExistException file

safeReadConf :: (MonadCatch m, MonadIO m) => m Conf
safeReadConf =
  readConf `catch`
    (\(SomeException a) -> do liftIO $ print a
                              return defaultConf)

sendCSVFileName, fileTreeDirectory :: (MonadCatch m, MonadIO m) => m FilePath
sendCSVFileName   = (^. #directoryFile) <$> safeReadConf
fileTreeDirectory = (^. #fileTree) <$> safeReadConf

directorySpecF, hihoSpecF, kumiaiSpecF, officeSpecF, kumiaiOfficeSpecF
  :: (MonadCatch m, MonadIO m) => m [Text]
directorySpecF    = (^. #directorySpec)    <$> safeReadConf
hihoSpecF         = (^. #hihoSpec)         <$> safeReadConf
kumiaiSpecF       = (^. #kumiaiSpec)       <$> safeReadConf
officeSpecF       = (^. #officeSpec)       <$> safeReadConf
kumiaiOfficeSpecF = (^. #kumiaiOfficeSpec) <$> safeReadConf

defaultConf :: Conf
defaultConf = #directoryFile @= "d:/送信案件一覧.csv"
              <: #directorySpec @= [ "事業所コード"
                                   , "事業所名"
                                   , "手続名"
                                   , "被保険者名"
                                   , "現在状況"
                                   , "社労士"]
              <: #hihoSpec         @= []
              <: #kumiaiSpec       @= []
              <: #officeSpec       @= []
              <: #kumiaiOfficeSpec @= []
              <: #fileTree         @= "y:/労働保険事務組合/電子申請e-Gov"
              <: #kumiaiFile       @= ""
              <: #officeFile       @= ""
              <: #kumiaiOfficeFile @= ""
              <: #hihoFile         @= ""
              <: #kumiaiDB         @= ""
              <: #officeDB         @= ""
              <: #kumiaiOfficeDB   @= ""
              <: #hihoDB           @= ""
              <: nil
