module Util.Telephone (telParse
                      , telFuncCore
                      , telFuncPure
                      , telString
                      , Telephone (..)
                      , fixFilter
                      , mobileFilter
                      , telMap
                      ) where

import Util                     ((++++), scan)
import Text.Parsec
import Text.Parsec.String

data Telephone =
  Fix String
  | Mobile String
  | Fax String deriving Show

instance Eq Telephone where
  Fix tel1 == Fix tel2 = toRegular tel1 == toRegular tel2
  Fax tel1 == Fax tel2 = toRegular tel1 == toRegular tel2
  Mobile tel1 == Mobile tel2 = toRegular tel1 == toRegular tel2
  _ == _ = False

telMap :: (String -> String) -> Telephone -> Telephone
telMap f (Fix s) = Fix $ f s
telMap f (Fax s) = Fax $ f s
telMap f (Mobile s) = Mobile $ f s

toRegular :: String -> String
toRegular s = case parse (scan digit) "" s of
  Right x -> x
  Left _  -> mempty

regularize :: Telephone -> Telephone
regularize = telMap toRegular

num :: String
num = ['0'..'9']

manyNoNum, manyNoNumPlus :: Parser String
manyNoNumPlus = many $ noneOf $ ['0'..'9'] ++ "-"
manyNoNum     = many $ noneOf num

----------------------------------------------------------------------------------------------------
fixParseHeader :: Parser String
fixParseHeader =
  try (string "06-")
  <|> string "07" ++++ ((:[]) <$> oneOf ['1'..'9'])

parenExp :: Parser String
parenExp = ("(" ++) . (++ ")") <$> core
  where core = between (char '(')
                       (char ')')
                       (many $ noneOf ")")

headStr :: [String] -> String
headStr [] = ""
headStr s  = head s

tailStr, tailFaxStr :: Parser String
tailStr    = headStr <$> (many parenExp <* manyNoNumPlus)
tailFaxStr = string "(F)" <* manyNoNumPlus

mobileParse, mobileParse2, fixParse, telFuncCore :: Parser Telephone
mobileParse = 
  Mobile <$> (manyNoNum *> count 3 digit)       ++++
             string "-"                         ++++
             count 4 digit                      ++++
             string "-"                         ++++
             count 4 digit                      ++++
             tailStr

mobileParse2 =
  Mobile <$> (manyNoNum *> count 3 digit)       ++++
             string "-"                         ++++
             count 8 digit                      ++++
             tailStr

fixParse = try fixParse3 <|> fixParse2

telFuncCore = try mobileParse
          <|> try mobileParse2
          <|> try fixParse
          <|> (anyChar >> telFuncCore)

telFuncPure = try mobileParse
              <|> try mobileParse2
              <|> try fixParse

fixNumberParse :: Parser String
fixNumberParse = try $ manyNoNum *> fixParseHeader ++++
                       count 9 (oneOf $ "-" ++ num)
             <|> try (manyNoNum *> count 3 digit   ++++
                      string "-"                   ++++
                      count 4 digit)

fixParse2, fixParse3 :: Parser Telephone
fixParse2 = Fix <$> fixNumberParse ++++ tailStr
fixParse3 = Fax <$> fixNumberParse ++++ tailFaxStr
  
telFunc :: Parser [Telephone]
telFunc = many $ try telFuncCore

telParse :: String -> [Telephone]
telParse s = either (const []) id $ parse telFunc "" s

telString :: Telephone -> String
telString (Fix s) = s
telString (Mobile s) = s
telString (Fax s) = s

fixFilter, mobileFilter :: [Telephone] -> [Telephone]
fixFilter    = filter (\n -> case n of Fix _ -> True; _ -> False)
mobileFilter = filter (\n -> case n of Mobile _ -> True; _ -> False)
