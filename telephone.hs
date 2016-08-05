module Telephone (telParse, telString, Telephone (..),
                 fixFilter, mobileFilter) where

import Text.Parsec
import Text.Parsec.String

data Telephone =
  Fix String
  | Mobile String
  | Fax String deriving (Eq, Show)

num :: [Char]
num = ['0'..'9']

manyNoNum, manyNoNumPlus :: Parser String
manyNoNumPlus = many $ noneOf $ ['0'..'9'] ++ "-"
manyNoNum     = many $ noneOf num

----------------------------------------------------------------------------------------------------
fixParseHeader :: Parser String
fixParseHeader =
  try (string "06-")
  <|> do { header <- string "07";
           nextch <- oneOf ['1'..'9'];
           return $ header ++ [nextch] }

parenExp :: Parser String
parenExp = core >>= return . ("(" ++) . (++ ")")
  where core = between (char '(')
                       (char ')')
                       (many $ noneOf ")")

faxExp :: Parser String
faxExp = string "(F)"

headStr :: [String] -> String
headStr [] = ""
headStr s  = head s

tailStr, tailFaxStr :: Parser String
tailStr    = headStr <$> (many parenExp <* manyNoNumPlus)
tailFaxStr = faxExp <* manyNoNumPlus

mobileParse, fixParse, telFuncCore :: Parser Telephone
mobileParse = do
  num1 <- manyNoNum *> count 3 digit <* char '-'
  num2 <- count 4 digit <* char '-'
  num3 <- count 4 digit
  exp' <- tailStr
  return $ Mobile (num1 ++ "-" ++ num2 ++ "-" ++ num3 ++ exp')

fixNumberParse :: Parser String
fixNumberParse =
  try (do { num1 <- manyNoNum *> fixParseHeader;
            rest <- count 9 (oneOf $ "-" ++ num);
            return $ num1 ++ rest })
  <|> try (do { num1 <- manyNoNum *> count 3 digit;
                sep  <- char '-';
                num2 <- count 4 digit;
                return $ num1 ++ [sep] ++ num2})

addParser :: Parser String -> Parser String -> Parser String
addParser f1 f2 = do
  x1 <- f1
  x2 <- f2
  return $ x1 ++ x2

fixParse2, fixParse3 :: Parser Telephone
fixParse2 = Fix <$> addParser fixNumberParse tailStr
fixParse3 = Fax <$> addParser fixNumberParse tailFaxStr 
  
fixParse = try fixParse3 <|> fixParse2

telFuncCore = do
  try mobileParse
  <|> try fixParse
  <|> (anyChar >> telFuncCore)

telFunc :: Parser [Telephone]
telFunc = many $ try telFuncCore

telParse :: String -> [Telephone]
telParse s = case parse telFunc "" s of
  Right x -> x
  Left _  -> []

telString :: Telephone -> String
telString (Fix s) = s
telString (Mobile s) = s
telString (Fax s) = s

fixFilter, mobileFilter :: [Telephone] -> [Telephone]
fixFilter    = filter (\n -> case n of Fix _ -> True; _ -> False)
mobileFilter = filter (\n -> case n of Mobile _ -> True; _ -> False)

