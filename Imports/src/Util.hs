{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  ) where

import RIO
import qualified Data.List            as DL
import qualified Data.Maybe           as M
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

data Pragma
data Import
data ImportQualified
data Other

data ParsedLine a =
  Pg [String]
  | Imp (String, Maybe String)
  | ImpQ (String, Maybe String)
  | ImpO String
  | O String deriving (Show)

pragmaParser :: P.Parser (ParsedLine Pragma)
pragmaParser = do
  Pg <$> P.between open close inner
  where
    open   = P.string "{-# LANGUAGE "
    close  = P.string " #-}"
    pragma = (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.noneOf ['#', ' ', ','])
    sep    = P.string ", "
    inner  = P.sepBy pragma sep

importNameParser :: P.Parser String
importNameParser = DL.intercalate "." <$> components
  where
    components = P.sepBy importNameComponentParser (P.string ".")

importNameComponentParser :: P.Parser String
importNameComponentParser =
  (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.oneOf ['A'..'z'])

importParser :: P.Parser (ParsedLine a)
importParser = do
  let header = P.string "import"
               >> P.many (P.char ' ')
               *> P.optionMaybe (P.string "qualified")
               <* P.many (P.char ' ')
  qlf  <- header
  name <- importNameParser
  rest <- P.optionMaybe (P.many (P.char ' ') >> P.many P.anyChar)
  case qlf of
    Just _  -> return $ ImpQ (name, rest)
    Nothing -> return $ Imp (name, rest)

otherParser :: P.Parser (ParsedLine Other)
otherParser = do
  rest <- P.many P.anyChar
  return $ O rest

lineParse :: P.Parser (ParsedLine a)
lineParse = do
  P.try importParser
  <|> P.try pragmaParser
  <|> otherParser

pragmas :: [ParsedLine a] -> (ParsedLine Pragma)
pragmas pl = pragmaAppend $ filter isPragma pl
  where
    isPragma (Pg _) = True
    isPragma _ = False
    Pg x1 <<>> Pg x2 = Pg (x1 ++ x2)
    pragmaAppend = DL.foldl' (<<>>) (Pg [])

-- toParsedLine :: String -> [ParsedLine]
-- toParsedLine target = rights $ map (P.parse lineParse "") $ lines target

plus2 :: Int -> Int
plus2 = (+ 2)

-- hoge = "{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}\n{-# LANGUAGE OverloadedStrings #-}\n \nimport RIO\nimport qualified Data.List            as DL\nimport qualified Data.Maybe           as M\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P"
