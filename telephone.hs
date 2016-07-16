module Telephone (telParse, telString, Telephone,
                 fixFilter, mobileFilter) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Text.Parsec.String

data Telephone =
  Fix String
  | Mobile String
  | Fax String deriving (Eq, Show)

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
parenExp = do
  char '('
  exp' <- many $ noneOf ")"
  char ')'
  return $ "(" ++ exp' ++ ")"

faxExp :: Parser String
faxExp = do { string "(F)"; return "(F)"}

headStr :: [String] -> String
headStr [] = ""
headStr s  = head s

tailStr :: Parser String
tailStr = do
  exp' <- many parenExp
  manyNoNumPlus
  return $ headStr exp'

tailFaxStr = do
  exp' <- faxExp
  manyNoNumPlus
  return $ exp'

mobileParse, fixParse, telFuncCore :: Parser Telephone
mobileParse = do
  num1 <- manyNoNum *> count 3 digit <* char '-'
  num2 <- count 4 digit <* char '-'
  num3 <- count 4 digit
  exp' <- tailStr
  return $ Mobile (num1 ++ "-" ++ num2 ++ "-" ++ num3 ++ exp')

fixNumberParse =
  try (do { num1 <- manyNoNum *> fixParseHeader;
            rest <- count 9 (oneOf $ "-" ++ num);
            return $ num1 ++ rest })
  <|> try (do { num1 <- manyNoNum *> count 3 digit;
                sep  <- char '-';
                num2 <- count 4 digit;
                return $ num1 ++ [sep] ++ num2})

fixParse2 = do
  num <- fixNumberParse
  exp <- tailStr
  return $ Fix $ num ++ exp

fixParse3 = do
  num <- fixNumberParse
  exp <- tailFaxStr
  return $ Fax $ num ++ exp
  
-- fixParse = try (do { num1 <- manyNoNum *> fixParseHeader;
--                      rest <- count 9 (oneOf $ "-" ++ num);
--                      exp' <- tailStr;
--                      return $ Fix $ num1 ++ rest ++ exp' })
--            <|> try (do { num1 <- manyNoNum *> count 3 digit;
--                          sep  <- char '-';
--                          num2 <- count 4 digit;
--                          exp' <- tailStr;
--                          return $ Fix $ num1 ++ [sep] ++ num2 ++ exp'})
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

