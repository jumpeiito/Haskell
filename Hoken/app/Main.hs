{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Util                   ((++++)
                                        , locEncoding
                                        , makeMap
                                        , scan
                                        , runFile
                                        , ketaNum
                                        , FileSystem (..)
                                        , latexCom)
import           Hoken.Base             (runXdoc, Person (..), config, MeiboMap)
import           Hoken.Parser           (pobjectParse, splitAddress)
import           Hoken.Meibo            (toLatex, toString, toDebug)
import           Util.Strdt             (getWeekDateString, strdt, toDay)
import qualified Meibo.Base             as Meibo
import           Data.Time
import           Data.Monoid
import           Data.Maybe             (fromMaybe, isJust)
import           Data.List              (isPrefixOf, intercalate, find)
import qualified Util.Telephone         as Tel
import qualified Data.Map               as Map
import           Control.Monad.Reader
import           Text.Parsec            hiding (Line, State)
import           Test.Hspec
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           Data.Yaml              hiding (Parser, Array)
import qualified System.IO              as I
import qualified Options.Applicative    as O

secondPrint :: [Person] -> Day -> IO ()
secondPrint persons d = do
  putStrLn $ "\\renewcommand{\\tempDay}{" ++ show (toDay d) ++ "}"
  putStrLn $ "\\renewcommand{\\tempDW}{" ++ getWeekDateString d ++ "}"

  forM_ persons $ \person -> 
    when (length (feeList person) == 3) $ 
      putStrLn $ toLatex person


debugPrint :: [Person] -> MeiboMap -> IO ()
debugPrint persons mmap = 
  forM_ persons $ \person -> 
    when (length (feeList person) == 3) $
      putStrLn $ toDebug person mmap

firstPrint :: [Person] -> MeiboMap -> IO ()
firstPrint persons mmap = 
  forM_ persons $ \person -> 
    when (length (feeList person) == 3) $
      putStrLn $ toString person mmap

main :: IO ()
main = do
  sjis <- I.mkTextEncoding "cp932"
  I.hSetEncoding I.stdout sjis
  I.hSetEncoding I.stdout I.utf8

  meibo <- Meibo.meiboMain "全" 

  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo

  let mmap = makeMap Meibo.bunkai id meibo

  let Just myDate = strdt (date' opt) :: Maybe Day
  
  output <- runXdoc (pdf opt) `runReaderT` config
  case parse (scan pobjectParse) "" output of
    Left _  -> return ()
    Right x -> 
      case (first' opt, second' opt) of
        (True, _) -> firstPrint x mmap
        (_, True) -> secondPrint x myDate
        (_, _)    -> debugPrint x mmap


data Secrets = S { secrets :: [[Text]] }

data Address = Ad { address :: [Text] }

instance FromJSON Address where
  parseJSON (Object v) = Ad <$> v .: "address"

instance FromJSON Secrets where
  parseJSON (Object v) = S <$> v .: "secrets"

-- "c:/Users/Jumpei/Haskell/Zipcode/address.yaml"
test2 :: IO ()
test2 = do
  Just file <- runFile $ File [ "d:/home/Haskell/Hoken/app/secret.yaml"
                              , "c:/Users/Jumpei/Haskell/Hoken/app/secret.yaml"]

  Just rc <- decodeFile file :: IO (Maybe Secrets)
  I.hSetEncoding I.stdout I.utf8
  -- mapM_ T.putStrLn $ secrets rc
  mapM_ print $ secrets rc

data Options = Options { first'  :: Bool
                       , second' :: Bool
                       , pdf     :: String
                       , date'   :: String
                       } deriving (Show)

firstP :: O.Parser Bool
firstP = O.switch $ O.short 'f' <> O.long "first" <> O.help ""

secondP :: O.Parser Bool
secondP = O.switch $ O.short 's' <> O.long "second" <> O.help ""

dateP :: O.Parser String
dateP = O.strOption $ mconcat
        [ O.short 'd', O.long "date"
        , O.help ""
        , O.metavar ""
        , O.value ""
        , O.showDefaultWith id]

pdfP :: O.Parser String
pdfP = O.strOption $ mconcat
        [ O.short 'p', O.long "pdf"
        , O.help ""
        , O.metavar ""
        , O.value ""
        , O.showDefaultWith id]

optionsP :: O.Parser Options
optionsP = (<*>) O.helper
           $ Options
           <$> firstP
           <*> secondP
           <*> pdfP
           <*> dateP

myParserInfo :: O.ParserInfo Options
myParserInfo = O.info optionsP $ mconcat 
    [ O.fullDesc
    , O.progDesc ""
    , O.header "Hoken-exe.exe"
    , O.footer ""
    , O.progDesc ""
    ]
