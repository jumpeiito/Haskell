{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Util
import qualified System.IO as I

run :: RIO App ()
run = do
  -- logInfo "We're inside the application!"
  contents <- liftIO $ I.hGetContents I.stdin
  liftIO $ output contents
