module Main where

import Match.Directory
import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  openPDFFileSexp (args !! 0) (args !! 1)
