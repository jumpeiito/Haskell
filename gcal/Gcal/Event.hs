{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Gcal.Event ( Reminder (..)
                  , Person (..)
                  , Default (..)
                  , GcalEvent (..)
                  , EventItem (..)
                  , EventDay (..)) where

import           GHC.Generics
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson 

data GcalEvent = GEV { grand_kind       :: String
                     , grand_etag       :: String
                     , grand_summary    :: String
                     , grand_updated    :: String
                     , grand_timeZone   :: String
                     , grand_accessRole :: String
                     , defaultReminders :: [Reminder]
                     , items            :: [EventItem]
                     } deriving (Show, Eq)

instance FromJSON Reminder
instance FromJSON Person
instance FromJSON Default
instance FromJSON GcalEvent where
  parseJSON (Object v) = GEV <$> v .: "kind"
                             <*> v .: "etag"
                             <*> v .: "summary"
                             <*> v .: "updated"
                             <*> v .: "timeZone"
                             <*> v .: "accessRole"
                             <*> v .: "defaultReminders"
                             <*> v .: "items"

instance FromJSON EventItem where
  parseJSON (Object v) =  Item <$> v .:  "kind"
                               <*> v .:  "etag"
                               <*> v .:  "id"
                               <*> v .:  "status"
                               <*> v .:  "htmlLink"
                               <*> v .:  "created"
                               <*> v .:  "updated"
                               <*> v .:  "summary"
                               <*> v .:? "description"
                               <*> v .:  "creator"
                               <*> v .:  "organizer"
                               <*> v .:  "start"
                               <*> v .:  "end"
                               <*> v .:  "iCalUID"
                               <*> v .:  "sequence"
                               <*> v .:  "reminders"
                               <*> v .:? "location"
                               <*> v .:? "colorId"
                               <*> v .:? "transparency"

instance FromJSON EventDay where
  parseJSON (Object v) =  ED <$> v .:? "date"
                             <*> v .:? "dateTime"

data Reminder = R { method  :: String
                  , minutes :: Int } deriving (Show, Eq, Generic)

data EventItem = Item { kind        :: String
                      , etag        :: String
                      , event_id    :: String
                      , status      :: String
                      , htmlLink    :: String
                      , created     :: String
                      , updated     :: String
                      , summary     :: String
                      , description :: Maybe String
                      , creator     :: Person
                      , organizer   :: Person
                      , start       :: EventDay
                      , end         :: EventDay
                      , iCalUID     :: String
                      , sequence    :: Int
                      , reminders   :: Default
                      , location    :: Maybe String
                      , colorID     :: Maybe String
                      , transparency :: Maybe String
                      } deriving (Show, Eq)

data Person = P { email       :: String
                , displayName :: String
                , self        :: Bool} deriving (Show, Eq, Generic)

data EventDay = ED { date     :: Maybe String
                   , dateTime :: Maybe String }
  deriving (Show, Eq)

data Default = D { useDefault :: Bool
                 , overrides  :: Maybe [Reminder] } deriving (Show, Eq, Generic)
