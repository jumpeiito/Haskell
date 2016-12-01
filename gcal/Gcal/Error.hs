module Gcal.Error where

import GHC.Generics

data Error = Error { error   :: ErrorContents
                   , code    :: Int
                   , message :: String } deriving (Show, Generic)

data ErrorContents = ErrC { errors :: ErrorExplanation }
  deriving (Show, Generic)

data ErrorExplanation = EEX { domain       :: String
                            , reason       :: String
                            , errMessage   :: String
                            , locationType :: String
                            , location     :: String }
  deriving (Show, Generic)
             
instance FromJSON Error
instance FromJSON ErrorContents
instance FromJSON ErrorExplanation
