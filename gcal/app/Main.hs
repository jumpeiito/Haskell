{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Main where

import GHC.Generics
import Data.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Conduit.List as CL
import Data.Aeson
import qualified Data.Conduit.Binary as CB
import Control.Arrow ((&&&))
import Util (withOutFile)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Maybe                     (fromMaybe)
import Network.URI
import Network.HTTP                   (simpleHTTP, getRequest, getResponseBody, RequestMethod (..))
import Network.HTTP.Base              (urlEncode, Request (..))
import Network.HTTP.Headers
import qualified System.IO as I
import System.Process (runInteractiveProcess)
import System.Directory (doesFileExist)

firefox = "c:/Program Files (x86)/Mozilla Firefox/firefox.exe"

data ClientJSON = ClientJSON { installed :: Keys } deriving (Show, Generic)
data Keys = K { client_id :: String
              , project_id :: String
              , auth_uri   :: String
              , token_uri  :: String
              , auth_provider_x509_cert_url :: String
              , client_secret :: String
              , redirect_uris :: [String] } deriving (Show, Generic)

data Token = Token { access_token :: String
                   , token_type   :: String
                   , expires_in   :: Int
                   , refresh_token :: String }
           | RefToken { access_token :: String
                      , token_type :: String
                      , expires_in :: Int}
           deriving (Show, Generic)


data Parameter a =
  ClientID a
  | ClientSecret a
  | RefreshToken a
  | GrantType a
  | ResponseType a
  | RedirectURI a
  | Scope a
  | Code a
  | AccessToken a
  | Key a
  | SingleEvents a
  | OrderBy a
  | TimeMin a
  | TimeMax a deriving (Show, Eq)


instance FromJSON ClientJSON
instance FromJSON Keys
instance FromJSON Token

clientID', clientSecret', authURI, tokenURI :: ClientJSON -> String
clientID'     = client_id     . installed
clientSecret' = client_secret . installed
authURI       = auth_uri      . installed
tokenURI      = token_uri     . installed

clientPair :: ClientJSON -> (ByteString, ByteString)
clientPair = (B.pack . clientID') &&& (B.pack . clientSecret')

loadJSON :: FromJSON a => FilePath -> IO (Maybe a)
loadJSON filepath = decode <$> BL.readFile filepath

makeParameter :: [(ByteString, ByteString)] -> ByteString
makeParameter = Data.List.foldl' conc mempty
  where conc seed (key, val)
          | seed == "" = "?" <> key <> "=" <> val
          | otherwise  = seed <> "&" <> key <> "=" <> val

refreshToken :: ClientJSON -> IO ()
refreshToken cj = runResourceT $ do
  manager <- liftIO $ newManager tlsManagerSettings
  
  para <- liftIO $ parseRequest "https://www.googleapis.com/oauth2/v3/token"

  let (id, sec) = clientPair cj
  let request = para { queryString = makeParameter [ ("client_id", id)
                                                   , ("client_secret", sec)
                                                   , ("refresh_token", "1/Rv4cYjyEHeoBR5b0pAL8CvfKry-O-BVg_IavYSq47Z0")
                                                   , ("grant_type", "refresh_token")]}

  let postRequest = urlEncodedBody [("status", "")] request
  response <- http postRequest manager
  responseBody response $$+- CB.sinkFile "test2.json"

requestAuhorization :: ClientJSON -> IO String
requestAuhorization cj = do
  let url = "https://accounts.google.com/o/oauth2/auth"
  let encode = B.pack . urlEncode . B.unpack
  let (id, _) = clientPair cj
  let params = makeParameter [ ("client_id", encode id)
                             , ("response_type", "code")
                             , ("redirect_uri", encode "urn:ietf:wg:oauth:2.0:oob")
                             , ("scope", encode "https://www.googleapis.com/auth/calendar")]
  runInteractiveProcess firefox [B.unpack (url <> params)] Nothing Nothing
  I.putStrLn "Enter the code your browser displayed: "
  I.hFlush I.stdout
  I.getLine

requestToken :: ClientJSON -> IO ()
requestToken cj = runResourceT $ do
  manager <- liftIO $ newManager tlsManagerSettings
  
  para <- liftIO $ parseRequest "https://www.googleapis.com/oauth2/v3/token"

  let (id, sec) = clientPair cj

  code <- liftIO $ requestAuhorization cj

  let request = para { queryString = makeParameter [ ("client_id", id)
                                                   , ("client_secret", sec)
                                                   , ("code", B.pack code)
                                                   , ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob")
                                                   , ("grant_type", "authorization_code")]}

  let postRequest = urlEncodedBody [("status", "")] request
  response <- http postRequest manager
  responseBody response $$+- CB.sinkFile "test3.json"

gcalEvent :: ClientJSON -> IO ()
gcalEvent cj = runResourceT $ do
  manager <- liftIO $ newManager tlsManagerSettings
  
  para <- liftIO $ parseRequest "https://www.googleapis.com/calendar/v3/calendars/junnpit@gmail.com/events"

  let (id, sec) = clientPair cj

  let request = para { queryString = makeParameter [ ("access_token", "ya29.Ci-kA4_mNShsGHtrmD7n147V7xUAaxM7rfNSgn_Q8lY2L3peMqauscQ3J0ttNQcb-A")
                                                   , ("key", sec)
                                                   , ("singleEvents", "True")
                                                   , ("orderBy", "startTime")
                                                   , ("timeMin", "2016-10-29T15:36:23Z")
                                                   , ("timeMax", "2017-01-27T15:36:23Z")
                                                   , ("grant_type", "authorization_code")]}

  response <- http request manager
  responseBody response $$+- CB.sinkHandle I.stdout


ensureToken :: ClientJSON -> IO (Maybe Token)
ensureToken clj = do
  let jsonFile = "test3.json"
  bool <- doesFileExist jsonFile
  if bool
    then loadJSON jsonFile
    else do { requestToken clj; loadJSON jsonFile }

-- "https://www.googleapis.com/calendar/v3/calendars/%s/events"

-- main :: IO ()
-- main = runResourceT $ do
--   manager <- liftIO $ newManager tlsManagerSettings
  
--   para <- liftIO $ parseRequest "https://www.googleapis.com/oauth2/v3/token"

--   let request = para { queryString = mconcat [ "?client_id=", "400386786048-f4djcafemof1dmh96fvtu6sebchm5mlj.apps.googleusercontent.com"
--                                              , "&client_secret=", "wNSuzJ0YBMj9j9_wXyoxpXFO"
--                                              , "&refresh_token=", "1/Rv4cYjyEHeoBR5b0pAL8CvfKry-O-BVg_IavYSq47Z0"
--                                              , "&grant_type=", "refresh_token"] }

--   let postRequest = urlEncodedBody [("status", "")] request
--   -- signedRequest <- signOAuth oauth credential postRequest
--   response <- http postRequest manager
--   responseBody response $$+- CB.sinkHandle I.stdout
