{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}
-- module Util.Tele (telParse
--                  , telFuncCore
--                  , telFuncPure
--                  , telString
--                  , Telephone (..)
--                  , fixFilter
--                  , mobileFilter
--                  , telMap
--                  ) where
module Util.Tele where

import Util                     ((++++))
import qualified Data.ByteString.Char8  as B
import Data.Monoid
import Control.Applicative
import Data.Attoparsec.ByteString hiding (satisfy, scan)
import Data.Attoparsec.ByteString.Char8 hiding (scan)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC

data Telephone =
  Fix B.ByteString
  | Mobile B.ByteString
  | Fax B.ByteString deriving Show

instance Eq Telephone where
  Fix tel1 == Fix tel2 = toRegular tel1 == toRegular tel2
  Fax tel1 == Fax tel2 = toRegular tel1 == toRegular tel2
  Mobile tel1 == Mobile tel2 = toRegular tel1 == toRegular tel2
  _ == _ = False

telMap :: (B.ByteString -> B.ByteString) -> Telephone -> Telephone
telMap f (Fix s) = Fix $ f s
telMap f (Fax s) = Fax $ f s
telMap f (Mobile s) = Mobile $ f s

scan :: Parser a -> Parser [a]
scan f1 = do
  try ((:) <$> f1 <*> scan f1)
  <|> (string "" >> return [])
  <|> (anyChar >> scan f1)

noneOf :: String -> Parser Char
noneOf c = satisfy (`notElem` c)

oneOf :: String -> Parser Char
oneOf xs = foldl1 (<|>) (map char xs)

between f1 f2 corep = f1 *> corep <* f2

toRegular :: B.ByteString -> B.ByteString
toRegular s = case parse (scan digit) s of
  Done _ x -> (B.pack x)
  _ -> mempty

regularize :: Telephone -> Telephone
regularize = telMap toRegular

num :: String
num = ['0'..'9']

manyNoNum, manyNoNumPlus :: Parser B.ByteString
manyNoNumPlus = B.pack <$> (many $ noneOf $ ['0'..'9'] ++ "-")
manyNoNum     = B.pack <$> (many $ noneOf num)

----------------------------------------------------------------------------------------------------
fixParseHeader :: Parser B.ByteString
fixParseHeader =
  try (string "06-")
  <|> ((<>) <$> string "07" <*> (B.singleton <$> oneOf ['1'..'9']))

parenExp :: Parser String
parenExp = ("(" ++) . (++ ")") <$> core
  where core = between (char '(')
                       (char ')')
                       (many $ noneOf ")")

headStr :: [String] -> B.ByteString
headStr [] = mempty
headStr s  = B.pack $ head s

tailStr, tailFaxStr :: Parser B.ByteString
tailStr    = headStr <$> (many parenExp <* manyNoNumPlus)
tailFaxStr = string "(F)" <* manyNoNumPlus

(<++>) f1 f2 = (<>) <$> f1 <*> f2
-- mobileParse, mobileParse2, fixParse, telFuncCore :: Parser Telephone
-- mobileParse = 
--   Mobile <$> (manyNoNum *> count 3 digit)       ++++
--              string "-"                         ++++
--              count 4 digit                      ++++
--              string "-"                         ++++
--              count 4 digit                      ++++
--              tailStr

-- mobileParse = do
--   parser <- (manyNoNum *> count 3 digit) <++>
--             string "-"
--   --           string "-"                   <++>
--   --           count 4 digit                <++>
--   --           string "-"                   <++>
--   --           count 4 digit                <++>
--   --           tailStr
--   return (Mobile $ B.pack parser)

-- mobileParse2 =
--   Mobile <$> (manyNoNum *> count 3 digit)       ++++
--              string "-"                         ++++
--              count 8 digit                      ++++
--              tailStr

-- fixParse = try fixParse3 <|> fixParse2

-- telFuncCore = try mobileParse
--           <|> try mobileParse2
--           <|> try fixParse
--           <|> (anyChar >> telFuncCore)

-- telFuncPure = try mobileParse
--               <|> try mobileParse2
--               <|> try fixParse

-- fixNumberParse :: Parser String
-- fixNumberParse = try $ manyNoNum *> fixParseHeader ++++
--                        count 9 (oneOf $ "-" ++ num)
--              <|> try (manyNoNum *> count 3 digit   ++++
--                       string "-"                   ++++
--                       count 4 digit)

-- fixParse2, fixParse3 :: Parser Telephone
-- fixParse2 = Fix <$> fixNumberParse ++++ tailStr
-- fixParse3 = Fax <$> fixNumberParse ++++ tailFaxStr

-- telFunc :: Parser [Telephone]
-- telFunc = many $ try telFuncCore

-- telParse :: String -> [Telephone]
-- telParse s = either (const []) id $ parse telFunc "" s

telString :: Telephone -> B.ByteString
telString (Fix s) = s
telString (Mobile s) = s
telString (Fax s) = s

fixFilter, mobileFilter :: [Telephone] -> [Telephone]
fixFilter    = filter (\n -> case n of Fix _ -> True; _ -> False)
mobileFilter = filter (\n -> case n of Mobile _ -> True; _ -> False)
