{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Util  where

import RIO
import qualified Data.List            as DL
import qualified System.IO            as I
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P

data Imports a =
  ImpN (a, Maybe a, Int) [a]
  | ImpNQ (a, Maybe a, Int) [a]
  deriving (Show, Eq)

data Pragma a = Pragma [a] Int
  deriving (Show, Eq)

data Parsed a = P (Pragma a) | I (Imports a) | O a | Newline
  deriving (Show, Eq)

data Header a = Header { contents        :: [Parsed a]
                       , importMaxLength :: Int }
  deriving Show

-- $setup
-- >>> import qualified Text.Parsec as P
-- >>> import qualified Text.Parsec.String as P

-- | Pragma Semigroup test
--
-- >>> Pragma ["LambdaCase", "FlexibleInstances"] 17 <> Pragma [] 0
-- Pragma ["LambdaCase", "FlexibleInstances"] 17
instance Semigroup (Pragma a) where
   Pragma x1 len1 <> Pragma x2 len2 =
     Pragma (x1 <> x2) (len1 `max` len2)

instance Monoid (Pragma a) where
  mempty  = Pragma [] 0
  mappend = (<>)

-- | blank parser test
--
-- >>> P.parse blank "" " "
-- Right " "
blank :: P.Parser String
blank = P.many $ P.char ' '

-- -- |
-- -- >>> P.parse pragmaParser "" "{-# LANGUAGE NoImplicitPrelude #-}"
-- -- Right (Pg ["NoImplicitPrelude"] 17)
-- -- >>> P.parse pragmaParser "" "{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}"
-- -- Right (Pg ["NoImplicitPrelude", "LambdaCase"] 17)
pragmaParser :: P.Parser (Pragma String)
pragmaParser = do
  prg <- P.between open close inner
  return $ Pragma prg (DL.maximum $ map length prg)
  where
    open   = P.string "{-# LANGUAGE" >> blank
    close  = blank >> P.string "#-}"
    pragma = (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.noneOf ['#', ' ', ','])
    sep    = P.string ", "
    inner  = P.sepBy pragma sep

pragmaLength :: Pragma a -> Int
pragmaLength (Pragma _ i) = i

importAddOther :: String -> Imports String -> Imports String
importAddOther other (ImpN a l) = ImpN a (l ++ [other])
importAddOther other (ImpNQ a l) = ImpNQ a (l ++ [other])

importNameParser :: P.Parser String
importNameParser = DL.intercalate "." <$> components
  where
    components = P.sepBy1 importNameComponentParser (P.string ".")

importNameComponentParser :: P.Parser String
importNameComponentParser =
  (:) <$> P.oneOf ['A'..'Z'] <*> P.many (P.oneOf ['A'..'z'])

importParser :: P.Parser (Imports String)
importParser = do
  qlf  <- P.string "import"
          >> blank
          *> P.optionMaybe (P.string "qualified")
          <* blank
  name <- importNameParser
  rest <- P.optionMaybe (blank >> P.many1 P.anyChar)
  let typeConst | isJust qlf = ImpNQ
                | otherwise  = ImpN
  return $ typeConst (name, rest, length name) mempty

newlineParser :: P.Parser (Parsed String)
newlineParser = blank >> P.eof >> return Newline

otherParser :: P.Parser (Parsed String)
otherParser = O <$> P.many P.anyChar

lineParse :: P.Parser (Parsed String)
lineParse =
  P.try (I <$> importParser)
  <|> P.try (P <$> pragmaParser)
  <|> P.try newlineParser
  <|> otherParser

toParsed :: Header String -> String -> Header String
toParsed h target =
  case P.parse lineParse "" target of
    Right (P p) ->
      case firstPragma $ contents h of
        Just fp ->
          h { contents = P (sortPragma (fp <> p)) : DL.tail (contents h) }
        Nothing ->
          h { contents = P p : contents h }
    Right (I i) ->
      h { contents = I i : contents h
        , importMaxLength = importLength i `max` importMaxLength h }
    Right (O s) ->
      case firstImports $ contents h of
        Just i  ->
          h { contents = I (importAddOther s i) : (DL.tail $ contents h) }
        Nothing ->
          h { contents = O s : contents h }
    Right r ->
      h { contents = r : contents h }
    Left _ ->
      h { contents = O target : contents h }

-- | Justify Test
--
-- >>> justify 10 "hoge"
-- "hoge      "
justify :: Int -> String -> String
justify n s = s ++ replicate (n - length s) ' '

isImport :: Parsed String -> Bool
isImport (I _)  = True
isImport _ = False

isPragma :: Parsed String -> Bool
isPragma (P _)  = True
isPragma _ = False

sortPragma :: Pragma String -> Pragma String
sortPragma (Pragma a i) = Pragma (DL.sort a) i

importLength :: Imports a -> Int
importLength (ImpN (_, _, len) _) = len
importLength (ImpNQ (_, _, len) _) = len

hasImports :: [Parsed String] -> Bool
hasImports = isJust . DL.find isImport

firstImports :: [Parsed String] -> Maybe (Imports String)
firstImports pl =
  case DL.find isImport pl of
    Just (I i) -> Just i
    Just _     -> Nothing
    Nothing    -> Nothing

firstPragma :: [Parsed String] -> Maybe (Pragma String)
firstPragma pl =
  case DL.find isPragma pl of
    Just (P p) -> Just p
    Just _     -> Nothing
    Nothing    -> Nothing

pragmaString :: Pragma String -> String
pragmaString (Pragma plist len) =
  DL.intercalate "\n" $ map makeString plist
  where
    makeString p = mconcat ["{-# ", justify len p, " #-}"]

importHeader :: Imports String -> String
importHeader (ImpN _ _)  = "import           "
importHeader (ImpNQ _ _) = "import qualified "

importRest :: Imports String -> String
importRest (ImpN (_, Just r, _) _)  = " " ++ r
importRest (ImpNQ (_, Just r, _) _) = " " ++ r
importRest _  = ""

importName :: Int -> Imports String -> String
importName maxlen (ImpN (n, _, _) _)  = justify maxlen n
importName maxlen (ImpNQ (n, _, _) _) = justify maxlen n

importStringInner :: Int -> [String] -> Imports String -> String
importStringInner maxlen other i =
  DL.intercalate "\n" stringList
  where
    stringList = [header i] ++ map lengthArrange other
    lengthArrange = (justify (maxlen + 18) "" ++) .
                    DL.dropWhile (== ' ')
    header = mconcat [ importHeader
                     , importName maxlen
                     , importRest ]

importString :: Int -> Imports String -> String
importString maxlen i@(ImpN _ o)  = importStringInner maxlen o i
importString maxlen i@(ImpNQ _ o) = importStringInner maxlen o i

headerOutput :: Header String -> [String]
headerOutput (Header c imaxlen) = map toString $ reverse c
  where
    toString = \case
      P p     -> pragmaString p
      Newline -> ""
      O s     -> s
      I i     -> importString imaxlen i

toHeader :: String -> Header String
toHeader = DL.foldl' toParsed (Header mempty 0) . lines

output :: String -> IO ()
output = mapM_ I.putStrLn . headerOutput . toHeader

hoge :: String
hoge = "{-# LANGUAGE Safe, NoImplicitPrelude, TemplateHaskell #-}\n{-# LANGUAGE OverloadedStrings                    #-}\n\nmodule Main where\nimport RIO\nimport qualified Data.List            as DL\nimport Data.List       ( intercalate\n               , sortBy)\nimport qualified Data.Maybe           as M\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P"

hoge2 :: String
hoge2 = "{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}\n{-# LANGUAGE FlexibleInstances              #-}\n-- | Silly utility module, used to demonstrate how to write a test\n-- case.\nmodule Util where\n\nimport   RIO\nimport qualified Data.List            as DL\nimport qualified System.IO            as I\nimport qualified Text.Parsec          as P\nimport qualified Text.Parsec.String   as P\n"
