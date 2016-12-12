{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeSynonymInstances #-}
-- I.hSetEncoding I.stdout I.utf8

module Gcal.Org where

import           Util                   (runFile, FileSystem (..))
import           Util.StrEnum           (split)
import           Data.Time
import           Data.Maybe             (fromJust)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import qualified Data.Conduit.Binary    as CB
import           Control.Monad.State
import           Gcal.Parameter         (makeParameter, Parameter (..))
import qualified Data.ByteString.Char8  as BC
import           Data.Aeson
import           Network.HTTP
import           Network.URI
import           Text.Parsec            hiding (State)
import           Text.Parsec.ByteString
import           Text.Read              (readMaybe)
import           Test.Hspec
import qualified System.IO              as I

orgDir :: FileSystem
orgDir = Directory [ "c:/Users/sibuc526.NEWNET/Dropbox/public/"
                   , "c:/Users/Jumpei/Dropbox/public/"]

orgFileSource :: MonadResource m => ConduitM () BC.ByteString m ()
orgFileSource = do
  Just directory <- liftIO $ runFile orgDir
  mconcat $ map (CB.sourceFile . (directory++)) ["schedule.org", "notes.org"]

orgTranslateConduit :: Monad m => Consumer BC.ByteString (ResourceT m) [Org]
orgTranslateConduit = CB.lines
                      $= CL.fold orgTranslateFold []
-- conduitTest = 
--   -- schedule.orgとnotes.orgのファイルの内容を連結し、出力する.
--   runResourceT $ orgFileSource $$ sink

hoge2 :: MonadIO m => Show a => Consumer a (ResourceT m) ()
hoge2 = do
  x <- CL.consume
  mapM_ (liftIO . print) x

conduitTest2 :: IO ()
conduitTest2 = do
  -- schedule.orgとnotes.orgのファイルの内容を連結した上で、Org型に変換し、print出力する。
  m <- runResourceT $ (orgFileSource $$ orgTranslateConduit)
  mapM_ (print . datetime) m

type OrgLevel   = Int
type HeaderInfo = (OrgLevel, Maybe OrgStatus, OrgString, Tags)
type Hour       = Int
type Minute     = Int
type Time       = (Day, Hour, Minute)
type Tags       = [OrgString]

data DateTime = Fix Time | Range Time Time
  deriving (Show, Eq)

data OrgTime = Normal DateTime
             | Deadline DateTime
             | Scheduled DateTime deriving (Show, Eq)

data Org = Org { level       :: OrgLevel
               , title       :: OrgString
               , status      :: Maybe OrgStatus
               , datetime    :: Maybe OrgTime
               , tags        :: [OrgString]
               , location    :: OrgString
               , description :: [OrgString]
               , children    :: [Org] } deriving (Show, Eq)

-- "{\"start\":{\"date\":\"2016-12-01\",\"dateTime\":null},\"end\":{\"date\":\"2016-12-02\",\"dateTime\":null},\"summary\":\"hoge\",\"location\":\"Kyoto City\",\"description\":\"buz\"}"

-- instance ToJSON BC.ByteString where
--   toJSON (BI.PS a _ _) = String a

instance ToJSON Org where
  toJSON (Org _ title status datetime _ location description _) =
    object [ "summary" .= unpack title
           , "location" .= unpack location
           , "description" .= (unpack $ BC.unlines description) ]

data OrgSymbolLine = OrgSymbolHeader HeaderInfo
                   | OrgSymbolTime OrgTime
                   | OrgLocation OrgString
                   | OrgDiscard
                   | OrgOther OrgString deriving (Show, Eq)

data OrgStatus = TODO | WAIT | DONE | SOMEDAY deriving (Show, Eq, Enum, Read)

----------------------------------------------------------------------------------------------------
type OrgString  = BC.ByteString
pack       = BC.pack
unpack     = BC.unpack
strLines   = BC.lines
strReverse = BC.reverse
----------------------------------------------------------------------------------------------------
choice2 :: Stream s m Char => [String] -> ParsecT s u m String
choice2 = choice . map string
----------------------------------------------------------------------------------------------------
classifyParse :: Parser OrgSymbolLine
classifyParse = 
  try (OrgSymbolHeader <$> headerParse)
  <|> try (OrgSymbolTime <$> timeParse)
  <|> try (OrgLocation <$> locationParse)
  <|> try (discardParse >> return OrgDiscard)
  <|> OrgOther <$> (pack <$> many anyChar)

readStatus :: String -> Maybe OrgStatus
readStatus = readMaybe

headerParse :: Parser HeaderInfo
headerParse = do
  stars  <- many1 (char '*') <* char ' '
  status <- try (choice2 ["TODO", "WAIT", "DONE", "SOMEDAY"]) <* char ' '
            <|> return ""
  rest   <- many (noneOf "*\n")
  let (title, tags) = splitHeader (pack rest)
  return ( length stars
         , readStatus status
         , title
         , tags)

splitSkipBlank :: OrgString -> Tags
splitSkipBlank = filter (/= "") . split ':'

splitHeaderParse :: Parser (OrgString, Tags)
splitHeaderParse = do
  tagstr <- try (char ':' >> many (noneOf "\n \t"))
            <|> return ""
  current <- getInput
  let cur  = strReverse current
  let tags = map strReverse $ splitSkipBlank (pack tagstr)
  return (cur, tags)

splitHeader :: OrgString -> (OrgString, Tags)
splitHeader str = either (const (str, [])) id $
                  parse splitHeaderParse "" (strReverse str)

makeOrg :: HeaderInfo -> Org
makeOrg (lev, stat, tit, tag) =
  Org { level       = lev
      , title       = tit
      , status      = stat
      , datetime    = Nothing
      , location    = ""
      , tags        = tag
      , description = []
      , children    = [] }

initOrg :: Org
initOrg = makeOrg (1, Nothing, mempty, mempty)

pushTime :: OrgTime -> [Org] -> [Org]
pushTime time (o:os) = o { datetime = Just time } : os
pushTime time [] = [ initOrg { datetime = Just time } ]

pushLocation :: OrgString -> [Org] -> [Org]
pushLocation loc (o:os) = o { location = loc } : os
pushLocation loc [] = [ initOrg { location = loc } ]

pushOther :: OrgString -> [Org] -> [Org]
pushOther txt (o:os) = o { description = description o ++ [txt] } : os
pushOther txt [] = [ initOrg { description = [txt] } ]
----------------------------------------------------------------------------------------------------
dayFromString :: String -> String -> String -> Day
dayFromString y m d = fromGregorian y' m' d'
  where y' = read y
        m' = read m
        d' = read d

timeBaseParse :: Parser Time
timeBaseParse = do
  _ <- oneOf "<["
  y <- count 4 digit <* char '-'
  m <- count 2 digit <* char '-'
  d <- count 2 digit <* many (noneOf "0123456789->]")
  (hour, minute) <- try $ do h' <- count 2 digit <* char ':'
                             m' <- count 2 digit
                             return (read h', read m')
                    <|> return (0, 0)
  _ <- oneOf ">]"
  return (dayFromString y m d, hour, minute)

timeParse :: Parser OrgTime
timeParse = do
  _     <- many (oneOf " \t")
  type' <- try (choice2 ["SCHEDULED:", "DEADLINE:"]) <|> return "NORMAL"
  _     <- many (noneOf "0123456789[<")
  st'   <- timeBaseParse
  time' <- try $ do en <- string "--" *> timeBaseParse
                    return $ Range st' en
           <|> return (Fix st')
  case type' of
    "NORMAL"      -> return $ Normal time'
    "SCHEDULED:"  -> return $ Scheduled time'
    "DEADLINE:"   -> return $ Deadline time'
    _ -> error "must not happen"
----------------------------------------------------------------------------------------------------
locationParse :: Parser OrgString
locationParse = 
  many (oneOf " \t") *>
  string ":LOCATION: " *>
  (pack <$> many (noneOf "\n"))
----------------------------------------------------------------------------------------------------
discardParse :: Parser OrgString
discardParse = do
  let text s = pack <$> string s
  let tryS s = try $ many (oneOf " \t") *> text s
  choice [ tryS ":PROPERTIES:", tryS ":END:", tryS ":LINK:"]
----------------------------------------------------------------------------------------------------
orgTranslateState :: [OrgString] -> State [Org] ()
orgTranslateState texts = 
  forM_ texts $ \tex -> do
    current <- get
    case parse classifyParse "" tex of
      Right (OrgSymbolHeader info) -> put (makeOrg info:current)
      Right (OrgSymbolTime otime)  -> put (pushTime otime current)
      Right (OrgLocation loc)      -> put (pushLocation loc current)
      Right (OrgOther txt)         -> put (pushOther txt current)
      Right OrgDiscard             -> return ()
      Left _ -> return ()
      
orgTranslateFold :: [Org] -> OrgString -> [Org]
orgTranslateFold xs line =
  case parse classifyParse "" line of
    Right (OrgSymbolHeader info) -> makeOrg info:xs
    Right (OrgSymbolTime otime)  -> pushTime otime xs
    Right (OrgLocation loc)      -> pushLocation loc xs
    Right (OrgOther txt)         -> pushOther txt xs
    Right OrgDiscard             -> xs
    Left _ -> xs

orgTranslate :: [OrgString] -> [Org]
orgTranslate texts = reverse . snd $ orgTranslateState texts `runState` []

orgRequest :: String -> String -> Org -> Request OrgString
orgRequest atoken key _ =
  Request { rqURI = puri
          , rqMethod = POST -- or Custom "PATCH"
          , rqHeaders = [ mkHeader HdrContentType "application/json"]
          , rqBody = "" }
  where uri = fromJust $ parseURI "https://www.googleapis.com/calendar/v3/calendars/junnpit@gmail.com/events"
        puri = uri { uriQuery = makeParameter [ AccessToken atoken
                                              , Key key
                                              , GrantType "authorization_code" ]}
----------------------------------------------------------------------------------------------------
orgSpec :: Spec
orgSpec = do
  let p' f = parse f ""
  --------------------------------------------------
  -- describe "headerParse" $ do
  --   it "matches Org-file headers" $
  --     p' headerParse "* hoge\n" `shouldBe` Right (1, "hoge")
  --   it "also matches more than 2-stars-header" $
  --     p' headerParse "** hoge\n" `shouldBe` Right (2, "hoge")
  --   it "also matches title which has some spaces and tabs" $
  --     p' headerParse "*** hoge foo\tbuz\n" `shouldBe` Right (3, "hoge foo\tbuz")
  --------------------------------------------------
  describe "readStatus" $ do
    it "" $ readStatus "TODO" `shouldBe` Just TODO
    it "" $ readStatus "WAIT" `shouldBe` Just WAIT
    it "" $ readStatus "DONE" `shouldBe` Just DONE
    it "" $ readStatus "SOMEDAY" `shouldBe` Just SOMEDAY
    it "" $ readStatus "" `shouldBe` Nothing
  describe "splitHeader" $ do
    it "" $ splitHeader "foobuz   :foo:buz:" `shouldBe`
      ("foobuz   ", ["buz", "foo"])
    it "" $ splitHeader "foobuz" `shouldBe`
      ("foobuz", [])
  describe "timeParse" $ do
    let maked y m d h mi = (fromGregorian y m d, h, mi)
    it "matches deadline-time" $
      p' timeParse "DEADLINE: <2015-03-24 Tue>" `shouldBe`
      Right (Deadline (Fix (maked 2015 3 24 0 0)))
    it "matches scheduled-time" $
      p' timeParse "SCHEDULED: <2016-12-15 Thu>" `shouldBe`
      Right (Scheduled (Fix (maked 2016 12 15 0 0)))
    it "matches [normal-formed-time]" $
      p' timeParse "[2016-12-03 Sat 17:53]" `shouldBe`
      Right (Normal (Fix (maked 2016 12 3 17 53)))
    it "matches <normal-formed-time>" $
      p' timeParse "<2016-11-25 Fri>" `shouldBe`
      Right (Normal (Fix (maked 2016 11 25 0 0)))
    it "matches deadline-range-time" $
      p' timeParse "   DEADLINE: <2015-03-24 Tue>--<2015-03-25 Wed>" `shouldBe`
      Right (Deadline (Range (maked 2015 3 24 0 0) (maked 2015 3 25 0 0)))
    it "matches scheduled-range-time" $
      p' timeParse " SCHEDULED: <2016-12-15 Thu>--<2016-12-16 Fri>" `shouldBe`
      Right (Scheduled (Range (maked 2016 12 15 0 0) (maked 2016 12 16 0 0)))
    it "matches [normal-formed-range-time]" $
      p' timeParse "  [2016-12-03 Sat 17:53]--[2016-12-03 Sat 20:00]" `shouldBe`
      Right (Normal (Range (maked 2016 12 3 17 53) (maked 2016 12 3 20 0)))
    it "matches <normal-formed-range-time>" $
      p' timeParse "    <2016-11-25 Fri>--<2016-11-27 Sun>" `shouldBe`
      Right (Normal (Range (maked 2016 11 25 0 0) (maked 2016 11 27 0 0)))
  --------------------------------------------------
  describe "classifyParse" $ do
    it "" $
      p' classifyParse "** TODO hoge foo, buz   :tiger:lion:" `shouldBe`
      Right (OrgSymbolHeader (2, Just TODO, "hoge foo, buz   ", ["lion", "tiger"]))
    it "" $
      p' classifyParse "*** buz-buz-buz buz" `shouldBe` Right (OrgSymbolHeader (3, Nothing, "buz-buz-buz buz", []))
    it "" $
      p' classifyParse " ** hoge foo, buz" `shouldBe` Right (OrgOther " ** hoge foo, buz")
    it "" $
      p' classifyParse ":PROPERTIES:" `shouldBe` Right OrgDiscard
    it "" $
      p' classifyParse " :END:" `shouldBe` Right OrgDiscard
    it "" $
      p' classifyParse "  \t:LINK:" `shouldBe` Right OrgDiscard
    it "" $
      p' classifyParse ":LOCATION: hoge" `shouldBe` Right (OrgLocation "hoge")
    it "" $
      p' classifyParse " :LOCATION: hoge" `shouldBe` Right (OrgLocation "hoge")
    it "" $
      p' classifyParse " \t:LOCATION: hoge" `shouldBe` Right (OrgLocation "hoge")
    it "" $
      p' classifyParse "  \t:LOCATION:hoge" `shouldBe` Right (OrgOther "  \t:LOCATION:hoge")
  
-- test = do
--   lines' <- Tx.lines <$> Txio.readFile "c:/Users/Jumpei/Haskell/gcal/Gcal/test.org"
--   mapM_ Txio.putStrLn lines'
