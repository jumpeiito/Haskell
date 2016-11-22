{-# LANGUAGE OverloadedStrings #-}

module Hoken.Secrets ( SecretPerson (..)
                     , secretMap
                     , toSecretPerson
                     , SecretMap
                     , Secrets (..)
                     , (<<|>>)) where
  
import           Util                   (runFile, FileSystem (..), makeSingleMap)
import           Control.Monad  
import           Data.Yaml              hiding (Parser, Array)
import           Data.Text              (Text)
import           Data.Maybe             (fromMaybe)
import qualified Data.Map               as Map
import qualified Data.Text.IO           as T
import           Text.StringLike        (castString)
import qualified System.IO              as I

data Secrets = S { secrets :: [[Text]] }
type SecretMap   = Map.Map String SecretPerson

data SecretPerson =
  SP { number :: String
     , name   :: String
     , post   :: String
     , ad1    :: String
     , ad2    :: String }
  | SPError deriving (Show, Eq)

instance FromJSON Secrets where
  parseJSON (Object v) = S <$> v .: "secrets"

toSecretPerson :: [Text] -> SecretPerson
toSecretPerson x@[_, _, _, _, _] =
  let [num', name', post', ad1', ad2'] = map castString x
  in SP num' name' post' ad1' ad2'
toSecretPerson _ = SPError  

(<<|>>) :: Maybe String -> Maybe String -> String
Just "" <<|>> Just x = x
Just x  <<|>> _ = x
Nothing <<|>> Just x = x
Nothing <<|>> _ = ""

secretMap :: IO SecretMap
secretMap = do
  Just file <- runFile $ File [ "d:/home/Haskell/Hoken/app/secret.yaml"
                              , "c:/Users/Jumpei/Haskell/Hoken/app/secret.yaml"]

  Just rc <- decodeFile file :: IO (Maybe Secrets)
  let gen = map toSecretPerson $ secrets rc
  return $ makeSingleMap number id gen
