{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util where

import RIO
import qualified Data.List            as DL
import qualified Data.Maybe           as M
import qualified System.IO            as I
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

data ParsedLine =
  Pg [String]
  | Imp (String, Maybe String)
  | ImpQ (String, Maybe String)
  | ImpO String
  | O String
  | Module
  | Newline
  deriving (Show)

pragmaParser :: P.Parser ParsedLine
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

importParser :: P.Parser ParsedLine
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

otherParser :: P.Parser ParsedLine
otherParser = do
  P.try (P.many (P.char ' ') >> P.eof >> return Newline)
  <|> (O <$> P.many P.anyChar)

lineParse :: P.Parser ParsedLine
lineParse = do
  P.try importParser
  <|> P.try pragmaParser
  <|> otherParser

pragmas :: [ParsedLine] -> ParsedLine
pragmas pl = pragmaSort $ pragmaAppend $ filter isPragma pl
  where
    isPragma (Pg _)   = True
    isPragma _        = False
    Pg x1 <<>> Pg x2  = Pg (x1 ++ x2)
    pragmaAppend      = DL.foldl' (<<>>) (Pg [])
    pragmaSort (Pg x) = Pg (DL.sort x)

pragmasText :: ParsedLine -> [String]
pragmasText (Pg x) = map toText x
  where
    toText pg  = mconcat ["{-# LANGUAGE ", justify' pg, " #-}"]
    maxLength  = DL.maximum $ map length x
    justify' pg = pg ++ (replicate (maxLength - length pg) ' ')

justify :: Int -> String -> String
justify n s = s ++ (replicate (n - length s) ' ')

imports :: [ParsedLine] -> [ParsedLine]
imports = foldr insert' []
  where
    insert' (O _) []    = []
    insert' (O "") seed = Newline : seed
    insert' (O s) seed  = ImpO s : seed
    insert' (Pg _) seed = seed
    insert' el seed     = el : seed

importsMaxLength :: [ParsedLine] -> Int
importsMaxLength pl = foldr maxlen 0 pl
  where
    maxlen Newline size          = size
    Imp (name, r) `maxlen` size  = length name `max` size
    ImpQ (name, r) `maxlen` size = length name `max` size
    ImpO _ `maxlen` size         = size

importsText :: [ParsedLine] -> [String]
importsText pl = foldr insert' [] pl
  where
    maxLength = importsMaxLength pl
    insert' el seed = toText el : seed
    toText Newline = "\n"
    toText (O s) = s
    toText (Imp (name, r)) =
      mconcat ["import           "
              , justify maxLength name
              , " "
              , fromMaybe "" r]
    toText (ImpQ (name, r)) =
      mconcat ["import qualified "
              , justify maxLength name
              , " "
              , fromMaybe "" r]
    toText (ImpO s) =
      mconcat [ justify (maxLength + 18) ""
              , DL.dropWhile (== ' ') s]


toParsedLine :: String -> [ParsedLine]
toParsedLine target = rights $ map (P.parse lineParse "") $ lines target

headerOutput :: String -> IO ()
headerOutput target = do
  let contents = toParsedLine target
  mapM_ I.print $ pragmasText $ pragmas contents
  mapM_ I.print $ importsText $ imports contents


plus2 :: Int -> Int
plus2 = (+ 2)

hoge = "{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}\n{-# LANGUAGE OverloadedStrings #-}\n\nmodule Main where\nimport RIO\nimport qualified Data.List            as DL\nimport Data.List       ( intercalate\n               , sortBy)\nimport qualified Data.Maybe           as M\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P"
