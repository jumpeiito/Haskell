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
                  , xdoc   :: FilePath
                  , secret :: FilePath}

data Person = P { number  :: String
                , name    :: String
                , phone   :: String
                , feeStr  :: String
                , feeSum  :: String
                , feeList :: [Integer] } deriving Show

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

telStringParse :: Parser String
telStringParse = do
  Tel.telString <$> Tel.telFuncPure
  <|> try (string " ")
  <|> string ""

hokenParse :: Parser String
hokenParse = do
  year'  <- count 1 digit
  shibu  <- string "醍"
  bunkai <- choice $ map (try . string) ["01", "02", "03", "04", "05", "50"]
  identy <- count 3 digit
  return $ year' ++ shibu ++ bunkai ++ identy

hokenFeeParse :: Parser String
hokenFeeParse = do
  try (string "00000")
  <|> try (string "0000")
  <|> try (string "000")
  <|> try (string "00")
  <|> try ((:) <$> digit <*> hokenFeeParse)

hokenFeeMany :: Parser String
hokenFeeMany = concat <$> many1 hokenFeeParse

testcase = "6醍01001京建太郎075-572-4949＊22000 22000*****6醍50101京花子090-1901-0111＊4120041200 82400"
testcase2 = "6醍50101京花子090-1901-0111＊4120041200 82400"
testcase3 = "6醍01001京建次郎 ＊22000 220006醍50101京花子090-1901-0111＊4120041200 82400" 

personParse :: Parser String
personParse = 
  hokenParse ++++
  many1 (noneOf "脱0123456789 ＊") ++++
  telStringParse ++++
  many (char '＊') ++++
  hokenFeeMany ++++ (string " ") ++++
  hokenFeeParse

pobjectParse :: Parser Person
pobjectParse = do
  num    <- hokenParse
  name'  <- many1 (noneOf "脱0123456789 ＊")
  tel    <- telStringParse
  _      <- many (char '＊')
  fee    <- hokenFeeMany <* string " "
  sum'   <- hokenFeeParse
  return $ P { number = num
             , name   = name'
             , phone  = tel
             , feeStr = fee
             , feeSum = sum'
             , feeList = []}

mainParse :: Parser [Person]
mainParse = do
  try ((:) <$> pobjectParse <*> mainParse)
  <|> (eof >> return [])
  <|> (anyChar >> mainParse)
  
personalSplit :: String -> String
personalSplit "" = ""
personalSplit s@(x:xs)
  | s `isPrefixOf` "6醍" = "\n6醍" ++ personalSplit (drop 2 s)
  | otherwise = s ++ personalSplit xs

main :: IO ()
main = do
  output <- runXdoc "f:/21_20160920.pdf" `runReaderT` config
  case parse mainParse "" output of
    Right x -> do
      I.hSetEncoding I.stdout I.utf8
      mapM_ (putStrLn . feeSum) x
    Left _  -> return ()
