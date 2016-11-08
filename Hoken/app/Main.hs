module Main where

import           Util                   ((++++), locEncoding)
import qualified Meibo.Base             as Meibo
import           Data.List              (isPrefixOf, intercalate)
import qualified Util.Telephone         as Tel
import           Control.Monad.Reader
import           Control.Monad.State
import           Text.Parsec            hiding (Line, State)
import           Text.Parsec.String
import           Test.Hspec
import           System.Process
import           System.Environment
import qualified System.IO              as I

data Config = Con { nkfwin :: FilePath
                  , xdoc   :: FilePath
                  , secret :: FilePath}

data Person = P { number  :: String
                , bunkai  :: String
                , name    :: String
                , phone   :: String
                , feeStr  :: String
                , feeSum  :: Int
                , feeList :: [Int] } deriving Show

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

_feeSplit :: String -> State (String, [String]) ()
_feeSplit str = do
  forM str $ \char -> do
    (fee, returner) <- get
    case (take 2 fee, char /= '0') of
      ("00", True) -> put ([char], reverse fee : returner)
      _            -> put (char:fee, returner)
  (fee, returner) <- get
  put ("", reverse $ reverse fee : returner)

feeSplit :: String -> [Int]
feeSplit str = map read $ snd (_feeSplit str `execState` ("", []))

numToBunkai :: String -> String
numToBunkai "01" = "石田"
numToBunkai "02" = "日野"
numToBunkai "03" = "小栗栖"
numToBunkai "04" = "一言寺"
numToBunkai "05" = "三宝院"
numToBunkai "50" = "点在"

pobjectParse :: Parser Person
pobjectParse = do
  num    <- hokenParse
  name'  <- many1 (noneOf "脱0123456789 ＊")
  tel    <- telStringParse
  _      <- many (char '＊')
  fee    <- hokenFeeMany <* string " "
  sum'   <- hokenFeeParse
  let feel = feeSplit fee
  let num' = drop 2 num
  return $ P { number = num'
             , bunkai = numToBunkai $ take 2 num'
             , name   = name'
             , phone  = tel
             , feeStr = fee
             , feeSum = sum feel
             , feeList = feel}

scan :: Parser a -> Parser [a]
scan f1 = do
  try ((:) <$> f1 <*> scan f1)
  <|> (eof >> return [])        -- 終了条件
  <|> (anyChar >> scan f1)
  
toString :: Person -> String
toString p = intercalate "," lists
  where lists = [ number p
                , bunkai p
                , name p
                , show $ feeSum p
                , show $ head $ feeList p
                , show $ feeList p ]

main :: IO ()
main = do
  sjis <- I.mkTextEncoding "cp932"
  I.hSetEncoding I.stdout sjis

  argv <- getArgs

  output <- runXdoc (argv!!0) `runReaderT` config
  case parse (scan pobjectParse) "" output of
    Left _  -> return ()
    Right x -> do
      I.hSetEncoding I.stdout I.utf8
      forM_ x $ \person -> do
        if length (feeList person) == 3
          then putStrLn $ toString person
          else return ()
