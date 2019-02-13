{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE LambdaCase              #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util where

import RIO
import qualified Data.List            as DL
import qualified System.IO            as I
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

data Parsed a =
  Pg [a] Int
  | Imp (a, Maybe a, Int)
  | ImpQ (a, Maybe a, Int)
  | ImpO a
  | O a
  | Newline
  deriving (Show)

data Header a = Header { contents        :: [Parsed a]
                       , pragmaMaxLength :: Int
                       , importMaxLength :: Int }
  deriving Show

blank :: P.Parser String
blank = P.many $ P.char ' '

pragmaParser :: P.Parser (Parsed String)
pragmaParser = do
  prg <- P.between open close inner
  return $ Pg prg (DL.maximum $ map length prg)
  where
    open   = P.string "{-# LANGUAGE" >> blank
    close  = blank >> P.string "#-}"
    pragma = (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.noneOf ['#', ' ', ','])
    sep    = P.string ", "
    inner  = P.sepBy pragma sep

importNameParser :: P.Parser String
importNameParser = DL.intercalate "." <$> components
  where
    components = P.sepBy1 importNameComponentParser (P.string ".")

importNameComponentParser :: P.Parser String
importNameComponentParser =
  (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.oneOf ['A'..'z'])

importParser :: P.Parser (Parsed String)
importParser = do
  qlf  <- P.string "import"
          >> blank
          *> P.optionMaybe (P.string "qualified")
          <* blank
  name <- importNameParser
  rest <- P.optionMaybe (blank >> P.many1 P.anyChar)
  case qlf of
    Just _  -> return $ ImpQ (name, rest, length name)
    Nothing -> return $ Imp (name, rest, length name)

newlineParser :: P.Parser (Parsed String)
newlineParser = blank >> P.eof >> return Newline

otherParser :: P.Parser (Parsed String)
otherParser = O <$> P.many P.anyChar

lineParse :: P.Parser (Parsed String)
lineParse = do
  P.try importParser
  <|> P.try pragmaParser
  <|> P.try newlineParser
  <|> otherParser

toParsed :: Header String -> String -> Header String
toParsed h target =
  case P.parse lineParse "" target of
    Right p@(Pg _ len) ->
      h { contents = p : contents h
        , pragmaMaxLength = len `max` pragmaMaxLength h }
    Right i@(Imp (_, _, len)) ->
      h { contents = i : contents h
        , importMaxLength = len `max` importMaxLength h }
    Right i@(ImpQ (_, _, len)) ->
      h { contents = i : contents h
        , importMaxLength = len `max` importMaxLength h }
    Right (O s) ->
      if hasImports $ contents h
      then h { contents = ImpO s : contents h }
      else h { contents = O s : contents h }
    Right r ->
      h { contents = r : contents h }
    Left _ ->
      h { contents = O target : contents h }

justify :: Int -> String -> String
justify n s = s ++ (replicate (n - length s) ' ')

isImport :: Parsed String -> Bool
isImport (Imp _)  = True
isImport (ImpQ _) = True
isImport _ = False

hasImports :: [Parsed String] -> Bool
hasImports = isJust . DL.find isImport

pragmaOutput :: Int -> String -> IO ()
pragmaOutput len p =
  I.putStrLn $ mconcat ["{-# ", justify len p, " #-}"]

importOutput :: Bool -> String -> [Char] -> Int -> IO ()
importOutput b n r len =
  I.putStrLn $ mconcat [ "import "
                       , if b then "qualified " else "          "
                       , justify len n
                       , " "
                       , r ]

headerOutput :: Header String -> IO ()
headerOutput (Header c pmaxlen imaxlen) = do
  forM_ (reverse c) $ \case
    Pg pgs _                -> forM_ pgs (pragmaOutput pmaxlen)
    Newline                 -> I.putStrLn ""
    O s                     -> I.putStrLn s
    Imp (name, Nothing, _)  -> I.putStrLn $ "import           " ++ name
    Imp (name, Just r, _)   -> importOutput False name r imaxlen
    ImpQ (name, Nothing, _) -> I.putStrLn $ "import qualified " ++ name
    ImpQ (name, Just r, _)  -> importOutput True name r imaxlen
    ImpO s                  ->
      I.putStrLn $ justify (imaxlen + 18) "" ++ DL.dropWhile (== ' ') s

toHeader :: String -> Header String
toHeader = DL.foldl' toParsed (Header mempty 0 0) . lines

output :: String -> IO ()
output = headerOutput . toHeader

hoge :: String
hoge = "{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}\n{-# LANGUAGE OverloadedStrings                    #-}\n\nmodule Main where\nimport RIO\nimport qualified Data.List            as DL\nimport Data.List       ( intercalate\n               , sortBy)\nimport qualified Data.Maybe           as M\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P"

hoge2 :: String
hoge2 = "{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}\n{-# LANGUAGE FlexibleInstances              #-}\n-- | Silly utility module, used to demonstrate how to write a test\n-- case.\nmodule Util where\n\nimport   RIO\nimport qualified Data.List            as DL\nimport qualified System.IO            as I\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P\n"
