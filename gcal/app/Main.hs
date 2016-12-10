{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Main where

import Gcal.Parameter (Parameter (..), makeParameter)
import Gcal.Request (getRequest, postRequest)
import Gcal.Event (GcalEvent (..))
import GHC.Generics
import Data.ByteString
import Data.Time
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
import Util.Strdt
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Data.Maybe                     (fromMaybe)
import Network.URI
import Network.HTTP                   (simpleHTTP, getResponseBody, RequestMethod (..))
import Network.HTTP.Base              (urlEncode, Request (..))
import           Text.Parsec            hiding (Line, State)
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
                   , refresh_token :: String } deriving (Show, Generic)

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

getRefreshToken :: IO (Maybe String)
getRefreshToken = do
  json <- loadJSON "test3.json"
  return $ refresh_token <$> json

getAccessToken :: IO (Maybe String)
getAccessToken = do
  json <- loadJSON "test3.json"
  return $ access_token <$> json

refreshToken :: ClientJSON -> IO ()
refreshToken cj = runResourceT $ do
  Just rtoken <- liftIO $ getRefreshToken

  let (id, sec) = clientPair cj
  let params = [ ClientID id
               , ClientSecret sec
               , RefreshToken (B.pack rtoken)
               , GrantType "refresh_token"]
  res <- postRequest "https://www.googleapis.com/oauth2/v3/token" params
  responseBody res $$+- CB.sinkFile "test2.json"

requestAuhorization :: ClientJSON -> IO String
requestAuhorization cj = do
  let url = "https://accounts.google.com/o/oauth2/auth"
  let encode = B.pack . urlEncode . B.unpack
  let (id, _) = clientPair cj
  let params = makeParameter [ ClientID (encode id)
                             , ResponseType "code"
                             , RedirectURI (encode "urn:ietf:wg:oauth:2.0:oob")
                             , Scope (encode "https://www.googleapis.com/auth/calendar")]
  runInteractiveProcess firefox [B.unpack (url <> params)] Nothing Nothing
  I.putStrLn "Enter the code your browser displayed: "
  I.hFlush I.stdout
  I.getLine

requestToken :: ClientJSON -> IO ()
requestToken cj = runResourceT $ do
  code     <- liftIO $ requestAuhorization cj
  let (id, sec) = clientPair cj
  let params = [ ClientID id
               , ClientSecret sec
               , Code (B.pack code)
               , RedirectURI "urn:ietf:wg:oauth:2.0:oob"
               , GrantType "authorization_code"]
  response <- postRequest "https://www.googleapis.com/oauth2/v3/token" params
  responseBody response $$+- CB.sinkFile "test3.json"

getTime :: IO (String, String)
getTime = do
  today <- todayDay
  let formatta d = (show d) ++ "T00:00:00Z"
  let f = formatta . addDays 60 &&& formatta . addDays (-60)
  return $ f today

gcalEvent :: ClientJSON -> IO ()
gcalEvent cj = runResourceT $ do
  Just atoken  <- liftIO $ getAccessToken
  (maxT, minT) <- liftIO $ getTime

  let (_, sec) = clientPair cj
  let params = [ AccessToken (B.pack atoken)
               , Key sec
               , SingleEvents "True"
               , OrderBy "startTime"
               , TimeMin (B.pack minT)
               , TimeMax (B.pack maxT)
               , GrantType "authorization_code"]
  response <- getRequest "https://www.googleapis.com/calendar/v3/calendars/junnpit@gmail.com/events" params
  -- 401が返ってきた場合、requestTokenを実行
  responseBody response $$+- CB.sinkHandle I.stdout

ensureToken :: ClientJSON -> IO (Maybe Token)
ensureToken clj = do
  let jsonFile = "test3.json"
  bool <- doesFileExist jsonFile
  if bool
    then loadJSON jsonFile
    else do { requestToken clj; loadJSON jsonFile }

main :: IO ()
main = return ()
