{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances              #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util where

import RIO
import qualified Data.List            as DL
import qualified System.IO            as I
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

data ParsedLine =
  Pg [String]
  | Imp (String, Maybe String)
  | ImpQ (String, Maybe String)
  | ImpO String
  | O String
  | Newline
  deriving (Show)

blank :: P.Parser String
blank = P.many $ P.char ' '

pragmaParser :: P.Parser ParsedLine
pragmaParser = do
  Pg <$> P.between open close inner
  where
    open   = P.string "{-# LANGUAGE" >> blank
    close  = blank >> P.string "#-}"
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
  qlf  <- P.string "import"
          >> blank
          *> P.optionMaybe (P.string "qualified")
          <* blank
  name <- importNameParser
  rest <- P.optionMaybe (blank >> P.many1 P.anyChar)
  case qlf of
    Just _  -> return $ ImpQ (name, rest)
    Nothing -> return $ Imp (name, rest)

otherParser :: P.Parser ParsedLine
otherParser = do
  P.try (blank >> P.eof >> return Newline)
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
    _     <<>> _      = error "must not happen at `pragmas`."
    pragmaAppend      = DL.foldl' (<<>>) (Pg [])
    pragmaSort (Pg x) = Pg (DL.sort x)
    pragmaSort _      = error "must not happen at `pragmas`."

pragmasText :: ParsedLine -> [String]
pragmasText (Pg x) = map toText x
  where
    toText pg  = mconcat ["{-# LANGUAGE ", justify' pg, " #-}"]
    maxLength  = DL.maximum $ map length x
    justify' pg = pg ++ (replicate (maxLength - length pg) ' ')
pragmasText _ = error "must not happen at `pragmasText`."

justify :: Int -> String -> String
justify n s = s ++ (replicate (n - length s) ' ')

hasImports :: [ParsedLine] -> Bool
hasImports []           = False
hasImports ((Imp _):_)  = True
hasImports ((ImpQ _):_) = True
hasImports ((ImpO _):_) = True
hasImports (_:xs)       = hasImports xs

imports :: [ParsedLine] -> [ParsedLine]
imports = reverse . DL.foldl' insert' []
  where
    insert' seed (O "") = Newline : seed
    insert' seed (O s)
      | hasImports seed = ImpO s : seed
      | otherwise       = O s : seed
    insert' seed (Pg _) = seed
    insert' seed el     = el : seed

importsMaxLength :: [ParsedLine] -> Int
importsMaxLength pl = foldr maxlen 0 pl
  where
    Newline        `maxlen` size = size
    O _            `maxlen` size = size
    Imp (name, _)  `maxlen` size = length name `max` size
    ImpQ (name, _) `maxlen` size = length name `max` size
    ImpO _         `maxlen` size = size
    _              `maxlen` _    = error "must not happen at `importsMaxLength`"

importsText :: [ParsedLine] -> [String]
importsText pl = foldr insert' [] pl
  where
    maxLength = importsMaxLength pl
    insert' el seed = toText el : seed
    toText Newline = "\n"
    toText (O s) = s
    toText (Pg _) = error "must not happen at `importsText`"
    toText (Imp (name, Nothing)) =
      mconcat ["import           " , name]
    toText (Imp (name, Just r)) =
      mconcat ["import           "
              , justify maxLength name , " " , r]
    toText (ImpQ (name, Nothing)) =
      mconcat ["import qualified " , name]
    toText (ImpQ (name, Just r)) =
      mconcat ["import qualified "
              , justify maxLength name , " " , r]
    toText (ImpO s) =
      mconcat [ justify (maxLength + 18) ""
              , DL.dropWhile (== ' ') s]

toParsedLine :: String -> [ParsedLine]
toParsedLine target = rights $ map (P.parse lineParse "") $ lines target

headerOutput :: String -> IO ()
headerOutput target = do
  let contents = toParsedLine target
  mapM_ I.putStrLn $ pragmasText $ pragmas contents
  mapM_ I.putStrLn $ importsText $ imports contents

plus2 :: Int -> Int
plus2 = (+ 2)

hoge :: String
hoge = "{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}\n{-# LANGUAGE OverloadedStrings                    #-}\n\nmodule Main where\nimport RIO\nimport qualified Data.List            as DL\nimport Data.List       ( intercalate\n               , sortBy)\nimport qualified Data.Maybe           as M\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P"
