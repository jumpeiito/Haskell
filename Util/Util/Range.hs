module Util.Range where

import Control.Applicative            ((<|>))
import Data.List                      (nub, sort)
import Test.Hspec
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

hasHyphenParser :: P.Parser [Int]
hasHyphenParser = do
  beg <- read <$> P.many1 P.digit <* (P.char '-')
  end <- read <$> P.many1 P.digit
  return $ [beg..end]

onlyNumParser :: P.Parser [Int]
onlyNumParser = do
  (:[]) <$> read <$> P.many1 P.digit

trimSpace :: P.Parser a -> P.Parser a
trimSpace p = do
  let space = P.many (P.char ' ')
  P.optional space
    >> p
    <* P.optional space

rangeParse :: P.Parser [Int]
rangeParse = do
  let parser =
        P.try (trimSpace hasHyphenParser)
        <|> (trimSpace onlyNumParser)
  let postFunc = sort . nub . mconcat
  let space  = P.many (P.char ' ') :: P.Parser String
  let sep    = P.char ','
  intList <- P.sepBy1 parser sep
  return $ postFunc intList

fromRange :: String -> Maybe [Int]
fromRange target =
  case P.parse rangeParse "" target of
    Right x -> Just x
    Left _  -> Nothing

spec :: Spec
spec = do
  describe "parser test" $ do
    it "rtest" $ do
      let parser = P.parse rangeParse ""
      parser "1-3"   `shouldBe` Right [1,2,3]
      parser "1"     `shouldBe` Right [1]
      parser "1,2,3" `shouldBe` Right [1,2,3]
      parser "1,3,4" `shouldBe` Right [1,3,4]
      parser "1"     `shouldBe` Right [1]
      parser "1-3, 10-12, 20-21"
        `shouldBe` Right [1,2,3,10,11,12,20,21]
      parser "1-9, 7-11, 8-12"
        `shouldBe` Right [1..12]
