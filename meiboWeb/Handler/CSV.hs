{-# LANGUAGE OverloadedStrings #-}
module Handler.CSV where

import Import
import System.Process (runInteractiveProcess)
import Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..), output)
import Util             (runRubyString)
import Text.StringLike                  (StringLike, castString)
import qualified System.IO as I
import qualified Data.Text.IO as Txio

getCSVR :: String -> HandlerT App IO String
getCSVR bunkai = do
  -- liftIO $ I.hSetBuffering I.stdout I.NoBuffering
  datalist <- liftIO $ meiboMain "全"
  return $ "(setq helm-meibo '(" ++ concatMap output datalist ++ "))"
