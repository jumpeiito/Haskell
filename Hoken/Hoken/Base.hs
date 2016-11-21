module Hoken.Base ( config
                  , runXdoc
                  , Person (..)
                  , MeiboMap) where

import Util.Telephone               (Telephone (..))
import Control.Monad.Reader
import System.Process               (runInteractiveProcess)
import qualified System.IO          as I
import qualified Data.Map           as Map
import qualified Meibo.Base         as Meibo

data Config = Con { nkfwin :: FilePath
                  , xdoc   :: FilePath
                  , secret :: FilePath}

data Person = P { number  :: String
                , bunkai  :: String
                , name    :: String
                , phone   :: Maybe Telephone
                , feeStr  :: String
                , feeSum  :: Int
                , feeList :: [Int] } deriving Show

type MeiboMap = Map.Map String [Meibo.Line]

config = Con { nkfwin = "f:/nkfwin.exe"
             , xdoc   = "d:/home/xdoc2txt/command/xdoc2txt.exe"
             , secret = "./.secret"}

runCom command fp =
  runInteractiveProcess command ["-o=1", fp] Nothing Nothing

runXdoc :: FilePath -> ReaderT Config IO String
runXdoc fp = do
  command         <- xdoc <$> ask
  (_, sout, _, _) <- liftIO $ runCom command fp
  liftIO $ I.hGetContents sout
