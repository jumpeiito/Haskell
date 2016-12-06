{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Gcal.Org where

import Data.Time
import Control.Monad.State
import qualified Data.Text as Tx
import qualified Data.Text.IO as Txio
import Text.Parsec hiding (State)
-- import Text.Parsec.String
import Text.Parsec.Text
import Test.Hspec

type OrgLevel = Int
type Hour     = Int
type Minute   = Int
type Time     = (Day, Hour, Minute)

data DateTime = Fix Time | Range Time Time
  deriving (Show, Eq)

data OrgTime = Normal DateTime
             | Deadline DateTime
             | Scheduled DateTime deriving (Show, Eq)

data Org = Org { level    :: OrgLevel
               , title    :: Tx.Text
               , datetime :: Maybe OrgTime
               , location :: Tx.Text
               , description :: [Tx.Text]
               , children :: [Org] }
           | OrgError ParseError deriving (Show, Eq)

data OrgSymbolLine = OrgSymbolHeader (OrgLevel, Tx.Text)
                   | OrgSymbolTime OrgTime
                   | OrgLocation Tx.Text
                   | OrgDiscard
                   | OrgOther Tx.Text deriving (Show, Eq)
-- testParse :: 
classifyParse :: Parser OrgSymbolLine
classifyParse = do
  try (OrgSymbolHeader <$> headerParse)
  <|> try (OrgSymbolTime <$> timeParse)
  <|> try (OrgLocation <$> locationParse)
  <|> try (discardParse >> return OrgDiscard)
  <|> OrgOther <$> (Tx.pack <$> (many anyChar))

headerParse :: Parser (OrgLevel, Tx.Text)
headerParse = do
  stars <- many1 (char '*') <* char ' '
  title <- many (noneOf "*\n")
  return (length stars, Tx.pack title)

initOrg :: Tx.Text -> Org
initOrg s = case parse headerParse "" s of
  Left s -> OrgError s
  Right (lev, tit) -> Org { level = lev
                          , title = tit
                          , datetime = Nothing
                          , location = ""
                          , description = []
                          , children = [] }

makeOrg :: Int -> Tx.Text -> Org
makeOrg lev tit = Org { level = lev
                      , title = tit
                      , datetime = Nothing
                      , location = ""
                      , description = []
                      , children = [] }

pushTime :: OrgTime -> [Org] -> [Org]
pushTime time (o:os) = o { datetime = Just time } : os

pushLocation :: Tx.Text -> [Org] -> [Org]
pushLocation loc (o:os) = o { location = loc } : os

pushOther :: Tx.Text -> [Org] -> [Org]
pushOther txt (o:os) = o { description = description o ++ [txt] } : os

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
  type' <- try (choice [string "SCHEDULED:", string "DEADLINE:"]) <|> return "NORMAL"
  _     <- many (noneOf "0123456789[<")
  st'   <- timeBaseParse
  time' <- try $ do en <- (string "--" *> timeBaseParse)
                    return $ Range st' en
           <|> (return $ Fix st')
  case type' of
    "NORMAL"      -> return $ Normal time'
    "SCHEDULED:"  -> return $ Scheduled time'
    "DEADLINE:"   -> return $ Deadline time'
----------------------------------------------------------------------------------------------------
locationParse :: Parser Tx.Text
locationParse = do
  many (oneOf " \t") *> string ":LOCATION: " *> (Tx.pack <$> many (noneOf "\n"))
----------------------------------------------------------------------------------------------------
discardParse :: Parser Tx.Text
discardParse = do
  let text s = Tx.pack <$> string s
  choice [ try $ many (oneOf " \t") *> text ":PROPERTIES:"
         , try $ many (oneOf " \t") *> text ":END:"]
----------------------------------------------------------------------------------------------------
orgTranslateState :: [Tx.Text] -> State [Org] ()
orgTranslateState texts = do
  forM_ texts $ \tex -> do
    current <- get
    case parse classifyParse "" tex of
      Right (OrgSymbolHeader (lev, tit)) -> put (makeOrg lev tit:current)
      Right (OrgSymbolTime otime)        -> put (pushTime otime current)
      Right (OrgLocation loc)            -> put (pushLocation loc current)
      Right (OrgDiscard)                 -> put current
      Right (OrgOther txt)               -> put (pushOther txt current)
      
orgTranslate :: [Tx.Text] -> [Org]
orgTranslate texts = reverse . snd $ orgTranslateState texts `runState` []

----------------------------------------------------------------------------------------------------
orgSpec :: Spec
orgSpec = do
  describe "headerParse" $ do
    it "matches Org-file headers" $
      parse headerParse "" "* hoge\n" `shouldBe` Right (1, "hoge")
    it "also matches more than 2-stars-header" $
      parse headerParse "" "** hoge\n" `shouldBe` Right (2, "hoge")
    it "also matches title which has some spaces and tabs" $
      parse headerParse "" "*** hoge foo\tbuz\n" `shouldBe` Right (3, "hoge foo\tbuz")
  describe "timeParse" $ do
    it "" $ parse timeParse "" "DEADLINE: <2015-03-24 Tue>" `shouldBe`
      Right (Deadline (Fix ((fromGregorian 2015 3 24), 0, 0)))
    it "" $ parse timeParse "" "SCHEDULED: <2016-12-15 Thu>" `shouldBe`
      Right (Scheduled (Fix ((fromGregorian 2016 12 15), 0, 0)))
    it "" $ parse timeParse "" "[2016-12-03 Sat 17:53]" `shouldBe`
      Right (Normal (Fix ((fromGregorian 2016 12 3), 17, 53)))
    it "" $ parse timeParse "" "<2016-11-25 Fri>" `shouldBe`
      Right (Normal (Fix ((fromGregorian 2016 11 25), 0, 0)))
    it "" $ parse timeParse "" "   DEADLINE: <2015-03-24 Tue>--<2015-03-25 Wed>" `shouldBe`
      Right (Deadline (Range ((fromGregorian 2015 3 24), 0, 0) ((fromGregorian 2015 3 25), 0, 0)))
    it "" $ parse timeParse "" " SCHEDULED: <2016-12-15 Thu>--<2016-12-16 Fri>" `shouldBe`
      Right (Scheduled (Range ((fromGregorian 2016 12 15), 0, 0) ((fromGregorian 2016 12 16), 0, 0)))
    it "" $ parse timeParse "" "  [2016-12-03 Sat 17:53]--[2016-12-03 Sat 20:00]" `shouldBe`
      Right (Normal (Range ((fromGregorian 2016 12 3), 17, 53) ((fromGregorian 2016 12 3), 20, 0)))
    it "" $ parse timeParse "" "    <2016-11-25 Fri>--<2016-11-27 Sun>" `shouldBe`
      Right (Normal (Range ((fromGregorian 2016 11 25), 0, 0) ((fromGregorian 2016 11 27), 0, 0)))
  
test = do
  lines' <- Tx.lines <$> Txio.readFile "c:/Users/Jumpei/Haskell/gcal/Gcal/test.org"
  mapM_ Txio.putStrLn lines'
