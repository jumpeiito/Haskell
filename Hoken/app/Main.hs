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
import           Util.Strdt             (getWeekDateString, strdt, toDay)
import qualified Util.Telephone         as Tel
import           Hoken.Base             (runXdoc, Person (..), config, MeiboMap)
import           Hoken.Parser           (pobjectParse, splitAddress)
import           Hoken.Meibo            (toLatex, toString, toDebug)
import qualified Hoken.Secrets          as Sec
import qualified Meibo.Base             as Meibo
import           Data.Time
import           Data.Monoid
import           Data.Maybe             (fromMaybe, isJust)
import           Data.List              (isPrefixOf, intercalate, find)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           Data.Yaml              hiding (Parser, Array)
import           Control.Monad.Reader
import           Text.Parsec            hiding (Line, State)
import           Test.Hspec
import qualified System.IO              as I
import qualified Options.Applicative    as O

secondPrint :: [Person] -> Day -> Sec.SecretMap -> IO ()
secondPrint persons d smap = do
  putStrLn $ "\\renewcommand{\\tempDay}{" ++ show (toDay d) ++ "}"
  putStrLn $ "\\renewcommand{\\tempDW}{" ++ getWeekDateString d ++ "}"

  forM_ persons $ \person -> do
    when (length (feeList person) == 3) $ do
      putStrLn $ toLatex person smap

debugPrint :: [Person] -> MeiboMap -> IO ()
debugPrint persons mmap = 
  forM_ persons $ \person -> 
    when (length (feeList person) == 3) $
      putStrLn $ toDebug person mmap

firstPrint :: [Person] -> MeiboMap -> Sec.SecretMap -> IO ()
firstPrint persons mmap smap = 
  forM_ persons $ \person -> do
    when (length (feeList person) == 3) $
      putStrLn $ toString person mmap smap

main :: IO ()
main = do
  sjis <- I.mkTextEncoding "cp932"
  I.hSetEncoding I.stdout sjis
  I.hSetEncoding I.stdout I.utf8

  meibo <- Meibo.meiboMain "全" 

  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo

  smap <- Sec.secretMap

  let mmap = makeMap Meibo.bunkai id meibo

  let Just myDate = strdt (date' opt) :: Maybe Day
  
  output <- runXdoc (pdf opt) `runReaderT` config
  case parse (scan pobjectParse) "" output of
    Left _  -> return ()
    Right x -> 
      case (first' opt, second' opt) of
        (True, _) -> firstPrint x mmap smap
        (_, True) -> secondPrint x myDate smap
        (_, _)    -> debugPrint x mmap


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
