module Main where

import Util                             ((++++))
import Data.List (isPrefixOf)
import qualified Util.Telephone         as Tel
import Control.Monad.Reader
import System.Process
import Text.Parsec                      hiding (Line, State)
import Text.Parsec.String
import Test.Hspec
import qualified System.IO              as I

data Config = Con { nkfwin :: FilePath
                  , xdoc   :: FilePath }

data Person = P { number  :: String
                , name    :: String
                , phone   :: String
                , feeStr  :: String
                , feeList :: [Integer] }

config = Con { nkfwin = "f:/nkfwin.exe"
             , xdoc   = "f:/xdoc2txt/command/xdoc2txt.exe" }

runCom command fp =
  runInteractiveProcess command ["-o=1", fp] Nothing Nothing

runXdoc :: FilePath -> ReaderT Config IO String
runXdoc fp = do
  command         <- xdoc <$> ask
  (_, sout, _, _) <- liftIO $ runCom command fp
  liftIO $ I.hGetContents sout

telStringParse :: Parser String
telStringParse = do
  Tel.telString <$> Tel.telFuncCore

hokenParse :: Parser String
hokenParse = do
  year'  <- count 1 digit
  shibu  <- string "醍"
  bunkai <- choice $ map (try . string) ["01", "02", "03", "04", "05", "50"]
  identy <- count 3 digit
  return $ year' ++ shibu ++ bunkai ++ identy

-- hokenFeeParse :: Parser String
-- hokenFeeParse = 
--   many1 digit  ++++
--   string "00 " ++++
--   many1 digit  ++++
--   string "00"

eofS :: Parser String
eofS = do
  _ <- eof
  return ""

hokenFeeParse :: Parser String
hokenFeeParse = do
  try (string "00 ")
  <|> try ((:) <$> digit <*> hokenFeeParse)
  <|> try eofS


personParse :: Parser String
personParse = 
  hokenParse ++++
  many (noneOf "脱0123456789") ++++
  telStringParse ++++
  many (char '＊') ++++
  hokenFeeParse ++++
  hokenFeeParse

personalSplit :: String -> String
personalSplit "" = ""
personalSplit s@(x:xs)
  | s `isPrefixOf` "6醍" = "\n6醍" ++ personalSplit (drop 2 s)
  | otherwise = s ++ personalSplit xs

main :: IO ()
main = do
  output <- runXdoc "f:/21_20160920.pdf" `runReaderT` config
  I.hSetEncoding I.stdout I.utf8
  putStrLn $ output
  


