module OpenPDF where

import Match.Directory
import System.Environment
import System.IO
import System.Process

acrord = "\"c:/Program Files/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\""

main :: IO ()
main = do
  args <- getArgs
  print args
  hFlush stdout
  openPDFFileFromString (head args)
