{-# LANGUAGE OverloadedStrings #-}

module Gcal.Request (postRequest, getRequest) where

import Data.ByteString                  (ByteString)
import Data.Conduit                     (ResumableSource)
import Gcal.Parameter                   (Parameter (..), makeParameter)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Trans.Resource     (MonadResource)
import Network.HTTP.Conduit             (Response, Request (..), newManager, tlsManagerSettings, parseRequest, http, urlEncodedBody)

makeRequest :: (MonadIO m, MonadResource m) =>
  (Request -> Request) ->       -- POST or GET
  String ->                     -- URL
  [Parameter ByteString] ->     -- Parameter
  m (Response (ResumableSource m ByteString))
makeRequest f url params = do
  manager <- liftIO $ newManager tlsManagerSettings
  req     <- liftIO $ parseRequest url
  let reqpara = req { queryString = makeParameter params }
  let postReq = f reqpara
  http postReq manager

postRequest :: (MonadIO m, MonadResource m) =>
  String -> [Parameter ByteString] -> m (Response (ResumableSource m ByteString))
postRequest = makeRequest (urlEncodedBody [("status", "")])

getRequest :: (MonadIO m, MonadResource m) =>
  String -> [Parameter ByteString] -> m (Response (ResumableSource m ByteString))
getRequest = makeRequest id
