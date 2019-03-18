module Match.Config
  ( PathGetter
  , Getter
  , Conf
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
import           Control.Lens             hiding (Getter)
import           Control.Monad.Trans
import           Data.Extensible
import           Data.Text                (Text)
import           Util.Yaml                (readYaml)

type Conf = Record
  '[ "directoryFile"    >: String
   , "directorySpec"    >: [Text]
   , "hihoSpec"         >: [Text]
   , "kumiaiSpec"       >: [Text]
   , "officeSpec"       >: [Text]
   , "officeSPSpec"     >: [Text]
   , "kumiaiOfficeSpec" >: [Text]
   , "fileTree"         >: String
   , "kumiaiFile"       >: String
   , "officeFile"       >: String
   , "officeSPFile"     >: String
   , "kumiaiOfficeFile" >: String
   , "hihoFile"         >: String
   , "kumiaiDB"         >: String
   , "officeDB"         >: String
   , "officeSPDB"       >: String
   , "kumiaiOfficeDB"   >: String
   , "hihoDB"           >: String
   ]

type Getter a = Getting a Conf a
type PathGetter = Getter FilePath

readConf :: (MonadThrow m, MonadIO m) => m Conf
readConf = readYaml "d:/home/matchConfig.yaml"

safeReadConf :: (MonadCatch m, MonadIO m) => m Conf
safeReadConf =
  readConf `catch`
    (\(SomeException a) -> do liftIO $ print a
                              return defaultConf)

sendCSVFileName, fileTreeDirectory :: (MonadCatch m, MonadIO m) => m FilePath
sendCSVFileName   = (^. #directoryFile) <$> safeReadConf
fileTreeDirectory = (^. #fileTree) <$> safeReadConf

directorySpecF, hihoSpecF, kumiaiSpecF, officeSpecF, kumiaiOfficeSpecF, officeSPSpecF :: (MonadCatch m, MonadIO m) => m [Text]
directorySpecF    = (^. #directorySpec)    <$> safeReadConf
hihoSpecF         = (^. #hihoSpec)         <$> safeReadConf
kumiaiSpecF       = (^. #kumiaiSpec)       <$> safeReadConf
officeSpecF       = (^. #officeSpec)       <$> safeReadConf
officeSPSpecF     = (^. #officeSPSpec)     <$> safeReadConf
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
              <: #officeSPSpec     @= []
              <: #kumiaiOfficeSpec @= []
              <: #fileTree         @= "y:/労働保険事務組合/電子申請e-Gov"
              <: #kumiaiFile       @= ""
              <: #officeFile       @= ""
              <: #officeSPFile     @= ""
              <: #kumiaiOfficeFile @= ""
              <: #hihoFile         @= ""
              <: #kumiaiDB         @= ""
              <: #officeDB         @= ""
              <: #officeSPDB       @= ""
              <: #kumiaiOfficeDB   @= ""
              <: #hihoDB           @= ""
              <: nil
