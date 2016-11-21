{-# LANGUAGE OverloadedStrings #-}

module Hoken.Secrets (SecretPerson (..)) where
  
import           Util                   (runFile, FileSystem (..), makeMap)
import           Data.Yaml              hiding (Parser, Array)
import           Data.Text              (Text)
import qualified Data.Map               as Map
import qualified Data.Text.IO           as T
import qualified System.IO              as I

data Secrets = S { secrets :: [[Text]] }
type HokenNumber = Text
type MeiboKey    = Text

data SecretPerson =
  SP { number :: Text
     , name   :: Text
     , post   :: Text
     , ad1    :: Text
     , ad2    :: Text }
  | SPError deriving (Show, Eq)

instance FromJSON Secrets where
  parseJSON (Object v) = S <$> v .: "secrets"

toSecretPerson :: [Text] -> SecretPerson
toSecretPerson [num', name', post', ad1', ad2'] =
  SP num' name' post' ad1' ad2'
toSecretPerson _ = SPError  

secretMap :: IO (Map.Map Text [SecretPerson])
secretMap = do
  Just file <- runFile $ File [ "d:/home/Haskell/Hoken/app/secret.yaml"
                              , "c:/Users/Jumpei/Haskell/Hoken/app/secret.yaml"]

  Just rc <- decodeFile file :: IO (Maybe Secrets)
  let gen = map toSecretPerson $ secrets rc
  return $ makeMap number id gen
