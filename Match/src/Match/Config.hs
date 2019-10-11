{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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
  , officeSPSpecF
  , kumiaiOfficeSpecF)
where

import           Control.Exception.Safe
import           Control.Lens             hiding (Getter)
import           Control.Monad.Trans
import           Data.Extensible
import           Data.Aeson
import           Data.Text                (Text)
import           GHC.Generics
import           Util.Yaml                (readYaml)

type Conf = Record
  '[ "directoryFile"    >: String
   , "directorySpec"    >: [Text]
   , "hihoSpec"         >: [Text]
   , "kumiaiSpec"       >: [Text]
   , "officeSpec"       >: [Text]
   , "officeSPSpec"     >: [Text]
   , "kumiaiOfficeSpec" >: [Text]
   , "hitoriSpec"       >: [Text]
   , "fileTree"         >: String
   , "kumiaiFile"       >: String
   , "officeFile"       >: String
   , "officeSPFile"     >: String
   , "kumiaiOfficeFile" >: String
   , "hihoFile"         >: String
   , "hitoriFile"       >: String
   , "hitori2File"      >: String
   , "kumiaiDB"         >: String
   , "officeDB"         >: String
   , "officeSPDB"       >: String
   , "kumiaiOfficeDB"   >: String
   , "hihoDB"           >: String
   , "hitoriDB"         >: String
   , "hitori2DB"        >: String
   ]

type Getter a = Getting a Conf a
type PathGetter = Getter FilePath

readConf :: (MonadThrow m, MonadIO m) => m Conf
readConf = readYaml "d:/home/temp/Haskell/Match/matchConfig.yaml"
-- readConf = readYaml "C:/users/Jumpei/Haskell/Match/matchConfig.yaml"

safeReadConf :: (MonadCatch m, MonadIO m) => m Conf
safeReadConf =
  readConf `catch`
    (\(SomeException a) -> do liftIO $ print a
                              return defaultConf)

sendCSVFileName, fileTreeDirectory :: (MonadCatch m, MonadIO m) => m FilePath
sendCSVFileName   = (^. #directoryFile) <$> safeReadConf
fileTreeDirectory = (^. #fileTree) <$> safeReadConf

directorySpecF    :: (MonadCatch m, MonadIO m) => m [Text]
hihoSpecF         :: (MonadCatch m, MonadIO m) => m [Text]
kumiaiSpecF       :: (MonadCatch m, MonadIO m) => m [Text]
officeSpecF       :: (MonadCatch m, MonadIO m) => m [Text]
kumiaiOfficeSpecF :: (MonadCatch m, MonadIO m) => m [Text]
officeSPSpecF     :: (MonadCatch m, MonadIO m) => m [Text]
hitoriSpecF       :: (MonadCatch m, MonadIO m) => m [Text]
directorySpecF    = (^. #directorySpec)    <$> safeReadConf
hihoSpecF         = (^. #hihoSpec)         <$> safeReadConf
kumiaiSpecF       = (^. #kumiaiSpec)       <$> safeReadConf
officeSpecF       = (^. #officeSpec)       <$> safeReadConf
officeSPSpecF     = (^. #officeSPSpec)     <$> safeReadConf
kumiaiOfficeSpecF = (^. #kumiaiOfficeSpec) <$> safeReadConf
hitoriSpecF       = (^. #hitoriSpec)       <$> safeReadConf

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
              <: #hitoriSpec       @= []
              <: #fileTree         @= "y:/労働保険事務組合/電子申請e-Gov"
              <: #kumiaiFile       @= ""
              <: #officeFile       @= ""
              <: #officeSPFile     @= ""
              <: #kumiaiOfficeFile @= ""
              <: #hihoFile         @= ""
              <: #hitoriFile       @= ""
              <: #hitori2File      @= ""
              <: #kumiaiDB         @= ""
              <: #officeDB         @= ""
              <: #officeSPDB       @= ""
              <: #kumiaiOfficeDB   @= ""
              <: #hihoDB           @= ""
              <: #hitoriDB         @= ""
              <: #hitori2DB        @= ""
              <: nil
