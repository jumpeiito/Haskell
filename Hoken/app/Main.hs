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
import qualified Meibo.Base             as Meibo
import           Data.Time
import           Data.Monoid
import           Data.Maybe             (fromMaybe, isJust)
import           Data.List              (isPrefixOf, intercalate, find)
import qualified Util.Telephone         as Tel
import qualified Data.Map               as Map
import           Control.Monad.Reader
import           Control.Monad.State
import           Text.Parsec            hiding (Line, State)
import           Text.Parsec.String
import           Test.Hspec
import           System.Process
import           System.Environment
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           Data.Yaml              hiding (Parser, Array)
import qualified System.IO              as I
import qualified Options.Applicative    as O

data Config = Con { nkfwin :: FilePath
                  , xdoc   :: FilePath
                  , secret :: FilePath}

data Person = P { number  :: String
                , bunkai  :: String
                , name    :: String
                , phone   :: Maybe Tel.Telephone
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

telStringParse :: Parser (Maybe Tel.Telephone)
telStringParse = do
  Just <$> Tel.telFuncPure
  <|> try (string " " >> return Nothing)
  <|> (string "" >> return Nothing)

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
testcase2 = "6醍50101伊東090-1901-0111＊4120041200 82400"
testcase3 = "6醍01001京建次郎 ＊22000 220006醍50101京花子090-1901-0111＊4120041200 82400" 

makeObjectSpec :: Spec
makeObjectSpec = do
  describe "pobjectParse running test" $ do
    let Right xs = parse pobjectParse "" testcase2
    it "hoken" $ number xs `shouldBe` "50101"
    it "name"  $ name  xs `shouldBe` "伊東"
    it "tel"   $ phone xs `shouldBe` Just (Tel.Mobile "090-1901-0111")

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

hasTel :: Tel.Telephone -> Meibo.Line -> Bool
hasTel telkey line = telkey `elem` Meibo.tel line
 
-- tmd :: Person -> [Meibo.Line] -> State 

toMeiboData :: Person -> Map.Map String [Meibo.Line] -> Maybe Meibo.Line
toMeiboData p mp = toMeiboData3 p mp `mplus` toMeiboData2 p mp

toMeiboData2 :: Person -> Map.Map String [Meibo.Line] -> Maybe Meibo.Line
toMeiboData2 p mp = case snd $ tmd p `runState` myMap of
  [x] -> Just x
  _   -> Nothing
  where Just myMap = Map.lookup (bunkai p) mp

tmd :: Person -> State [Meibo.Line] ()
tmd p = do
  let name' = name p
  forM_ name' $ \char -> do
    target <- get
    case filter ((char `elem`) . Meibo.name) target of
      []  -> put target
      x   -> put x

toMeiboData3 :: Person -> Map.Map String [Meibo.Line] -> Maybe Meibo.Line
toMeiboData3 p mp =
  let Just targetList = Map.lookup (bunkai p) mp
  in case phone p of
    Nothing     -> Nothing
    Just telnum ->
      let telnum' | "075-" `isPrefixOf` Tel.telString telnum = drop 4 `Tel.telMap` telnum
                  | otherwise = telnum
      in find (hasTel telnum') targetList

toLatex :: Person -> String
toLatex p = "\\Joseki{" ++ name' ++ "}{" ++ sum' ++ "}{" ++ head' ++ "}"
  where name' = name p
        sum'  = ketaNum $ show $ feeSum p
        head' = ketaNum $ show $ head $ feeList p

toString :: Person -> Map.Map String [Meibo.Line] -> String
toString p mp = latexCom "personallabel" arguments
  where arguments = [ pt, ad1, ad2, name p ]
        meiboData = toMeiboData p mp
        ad = fromMaybe "" $ Meibo.ad <$> meiboData
        pt = fromMaybe "" $ Meibo.postal <$> meiboData
        (ad1, ad2) = splitAddress ad

toDebug :: Person -> Map.Map String [Meibo.Line] -> String
toDebug p mp = latexCom "debug" arguments
  where arguments = [ pt
                    , ad1
                    , ad2
                    , name p
                    , number p
                    , bunkai p
                    , feeStr p
                    , show $ feeSum p]
        meiboData = toMeiboData p mp
        ad = fromMaybe "" $ Meibo.ad <$> meiboData
        pt = fromMaybe "" $ Meibo.postal <$> meiboData
        (ad1, ad2) = splitAddress ad
        

takeWhileP f = do
  (:) <$> f <*> takeWhileP f
  <|> return []

splitAddressParser :: Parser (String, String)
splitAddressParser = do
  let num = "0123456789-"
  addr  <- takeWhileP (noneOf num)
  numb  <- takeWhileP (oneOf num)
  other <- try (many1 anyChar) <|> (eof >> return "")
  return (addr ++ numb, other)

splitAddress :: String -> (String, String)
splitAddress ad = case parse splitAddressParser "" ad of
                    Right x -> x
                    Left _  -> (ad, "")

secondPrint :: [Person] -> IO ()
secondPrint persons = do
  forM_ persons $ \person -> do
    if length (feeList person) == 3
      then putStrLn $ toLatex person
      else return ()

debugPrint :: [Person] -> Map.Map String [Meibo.Line] -> IO ()
debugPrint persons mmap = do
  forM_ persons $ \person -> do
    if length (feeList person) == 3
      then putStrLn $ toDebug person mmap
      else return ()

firstPrint :: Day -> [Person] -> Map.Map String [Meibo.Line] -> IO ()
firstPrint d persons mmap = do
  putStrLn $ "\\renewcommand{\\tempDay}{" ++ show (toDay d) ++ "}"
  putStrLn $ "\\renewcommand{\\tempDW}{" ++ getWeekDateString d ++ "}"

  forM_ persons $ \person -> do
    if length (feeList person) == 3
      then putStrLn $ toString person mmap
      else return ()

main :: IO ()
main = do
  sjis <- I.mkTextEncoding "cp932"
  I.hSetEncoding I.stdout sjis
  I.hSetEncoding I.stdout I.utf8
  argv  <- getArgs
  meibo <- Meibo.meiboMain "全" 

  opt <- O.customExecParser (O.prefs O.showHelpOnError) myParserInfo

  let mmap = makeMap Meibo.bunkai id meibo

  let Just myDate = strdt (date' opt) :: Maybe Day
  
  output <- runXdoc (pdf opt) `runReaderT` config
  case parse (scan pobjectParse) "" output of
    Left _  -> return ()
    Right x -> do
      case (first' opt, second' opt) of
        (True, _) -> firstPrint myDate x mmap
        (_, True) -> secondPrint x
        (_, _)    -> debugPrint x mmap

-- telStringParseSpec :: Spec
-- telStringParseSpec = do
--   describe "telStringParse" $ do
--     it "matches 0xx-xxx-xxxx (fixed) style." $
--       parse telStringParse "" "075-572-4949" `shouldBe` Right "075-572-4949"
--     it "matches 0xxx-xx-xxxx (fixed) style." $
--       parse telStringParse "" "0774-22-2222" `shouldBe` Right "0774-22-2222"
--     it "matches xxx-xxxx (fixed) style." $
--       parse telStringParse "" "572-4949" `shouldBe` Right "572-4949"
--     it "matches 090-xxxx-xxxx (mobile) style." $
--       parse telStringParse "" "090-0000-0000" `shouldBe` Right "090-0000-0000"
--     it "matches 080-xxxx-xxxx (mobile) style." $
--       parse telStringParse "" "080-9999-9999" `shouldBe` Right "080-9999-9999"
--     it "matches 090-xxxxxxxx (mobile) style." $
--       parse telStringParse "" "090-00000000" `shouldBe` Right "090-00000000"
--     it "matches a space character." $
--       parse telStringParse "" " " `shouldBe` Right " "
--     it "matches digit chars when surrounded with non-numeric chars." $
--       parse telStringParse "" "hoge080-9999-9999foo" `shouldBe` Right "080-9999-9999"
--     it "matches digit chars when surrounded with non-numeric chars." $
--       parse telStringParse "" "buz/080-9999-9999-soo" `shouldBe` Right "080-9999-9999"
test = do
  meibo <- Meibo.meiboMain "全" 
  let mmap = makeMap Meibo.bunkai id meibo
  case parse pobjectParse "" testcase2 of
    Right x -> print $ toMeiboData x mmap
    Left _  -> return ()

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
