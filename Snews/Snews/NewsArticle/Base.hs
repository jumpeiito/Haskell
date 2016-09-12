{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Snews.NewsArticle.Base ( URL
                              , ArticleKey
                              , DirectionType
                              , AKey (..)
                              , WriterDirection (..)
                              , Config (..)
                              , URLParts (..)
                              , makeURL
                              , findTree
                              , findTreeS
                              , findAttribute
                              , findAttributeS
                              , (<~)
                              , (<~~)
                              , stringFoldBase
                              , treeText
                              , treeTextEx
                              , normalDirection
                              , filterBlankLines
                              , translateTags
                              , utf8Text
                              , takeTitle
                              , takeText) where

import Util
import Util.StrEnum
import Data.Time                        (Day (..))
import Data.List                        (foldl', isInfixOf)
import Data.Text.Internal               (Text (..))
import Data.Text.Encoding               (decodeUtf8)
import Control.Monad.State              (get, put, State, runState, execState)
import Control.Monad.Reader
import Control.Monad.Writer
import Text.StringLike                  (StringLike, castString)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec                      hiding (State)
import Text.Parsec.String
import qualified Data.Text              as Tx

type URL             = String
type ArticleKey      = (AKey, AKey)
type DirectionList   = [(AKey, AKey, WriterDirection)]
type DirectionType a = [(TagTree a -> Bool, WriterDirection)]

data AKey = Name String | Attr String | Always deriving (Show, Eq)

data WriterDirection = Skip | Pack String | Loop deriving (Show, Eq)

data Config a = Con { hostName  :: String
                    , baseName  :: String
                    , rootAK    :: [ArticleKey]
                    , titleAK   :: [ArticleKey]
                    , textAK    :: [ArticleKey]
                    , findFunc  :: [ArticleKey] -> a -> [TagTree Text]
                    , direct    :: DirectionList
                    , urlRecipe :: [URLParts] }
----------------------------------------------------------------------------------------------------
data URLParts =
  Host
  | Base
  | MDay (Day -> String)
  | Str String
  | Slash URLParts

makeURL :: Day -> Reader (Config a) String
makeURL d = do
  recipe <- urlRecipe <$> ask
  host   <- hostName <$> ask
  base   <- baseName <$> ask
  let murl gen (Slash x) = gen ++ murl "" x ++ "/"
      murl gen  Host     = gen ++ host
      murl gen  Base     = gen ++ base
      murl gen (MDay f)  = gen ++ f d
      murl gen (Str s)   = gen ++ s
  return $ foldl murl "" recipe
----------------------------------------------------------------------------------------------------
findTree :: StringLike a => [ArticleKey] -> TagTree a -> [TagTree a]
findTree akeys tb@TagBranch{} = execWriter $ find' akeys tb
  where find' _ (TagLeaf _)   = tell mempty
        find' akeys tb@(TagBranch _ _ ys)
          | matchTree akeys tb = tell [tb]
          | otherwise       = forM_ ys (tell . findTree akeys)
findTree _ _ = []

findTreeS, (<~) :: StringLike a => [ArticleKey] -> [TagTree a] -> [TagTree a]
findTreeS ak = (findTree ak `concatMap`)
(<~) = findTreeS

infixr 9 <~
infixr 9 <~~

matchTree :: StringLike a => [ArticleKey] -> TagTree a -> Bool
matchTree akeys = foldl' (|||) (const False) $ map logicProduct akeys

logicProduct :: StringLike a => ArticleKey -> TagTree a -> Bool
logicProduct (l, r) = matchAKey l &&& matchAKey r

matchAKey :: StringLike a => AKey -> TagTree a -> Bool
matchAKey (Name a) (TagBranch n _ _) = a == castString n
matchAKey (Attr a) (TagBranch _ l _) = [pairF castString ("class", a)] `isInfixOf` l
matchAKey Always _                   = True
matchAKey _ _                        = False

pairF :: (t -> t1) -> (t, t) -> (t1, t1)
pairF f (a, b) = (f a, f b)
----------------------------------------------------------------------------------------------------
findAttribute :: Eq s => s -> TagTree s -> [s]
findAttribute key (TagBranch _ attr tbs) = execWriter fA
  where fA = do
          case assocKey key attr of
            Just y -> tell [y]
            _      -> tell mempty
          forM_ tbs (tell . findAttribute key)
findAttribute _ _ = mempty

findAttributeS, (<~~) :: Eq s => s -> [TagTree s] -> [s]
findAttributeS key = (findAttribute key `concatMap`)
(<~~) = findAttributeS

assocKey :: Eq a => a -> [(a, b)] -> Maybe b
assocKey _ [] = Nothing
assocKey k ((x, y):rest)
  | k == x = Just y
  | otherwise = assocKey k rest
----------------------------------------------------------------------------------------------------
stringFoldBase :: Text -> Text
stringFoldBase tx = Tx.pack "   " <> stringFold 33 "\n   " tx
----------------------------------------------------------------------------------------------------
treeText    :: (StringLike a, Monoid a) => TagTree a -> a
treeTextMap :: (StringLike a, Monoid a) => [TagTree a] -> a
treeText    = treeTextEx normalDirection
treeTextMap = mconcat . map treeText
----------
normalDirection :: DirectionList
normalDirection = 
  [(Name "script", Always,          Skip),
   (Name "div",    Attr "posted",   Skip),
   (Name "div",    Attr "bookmark", Skip),
   (Name "p",      Attr "date",     Skip),
   (Name "br",     Always,          Pack "\n"),
   (Always,        Always,          Loop)]

directionTranslate :: StringLike a => DirectionList -> DirectionType a
directionTranslate = map translate'
  where translate' (n, a, d) = (logicProduct (n, a), d)

directionList :: StringLike a => DirectionType a
directionList = directionTranslate normalDirection

-- direction :: StringLike a =>
--              DirectionType a -> [(DirectionType a -> Bool, WriterDirection)] -> WriterDirection
direction _ [] = Skip
direction tb ((f, direct):xs)
  | f tb      = direct
  | otherwise = direction tb xs
----------------------------------------------------------------------------------------------------
treeTextEx :: (StringLike a, Monoid a) => DirectionList -> TagTree a -> a
treeTextEx dl = execWriter . ttxex dl

ttxex :: (StringLike a, Monoid a) => DirectionList -> TagTree a -> Writer a ()
ttxex _ (TagLeaf (TagText s)) = tell s
ttxex dl tb@(TagBranch _ _ descend) =
  case direction tb dx of
    Skip   -> tell mempty
    Pack n -> tell $ castString n
    Loop   -> forM_ descend (tell . treeTextEx dl)
  where dx = directionTranslate dl
ttxex _ _ = tell mempty
----------------------------------------------------------------------------------------------------
strip :: StringLike a => a -> Text
strip = tailCut . skip . (<> Tx.pack "\n") . decode
  where decode  = decodeUtf8 . castString
        skip    = Tx.dropWhile (`elem` [' ', '\t', '\12288', '\n'])
        tailCut = Tx.reverse . skip . Tx.reverse

filterBlankLines :: (StringLike a, Monoid a) => [a] -> [Text]
filterBlankLines [] = []
filterBlankLines (x:xl) = case parse fBLparse "" str of
  Right _ -> filterBlankLines xl
  Left _  -> strip x : filterBlankLines xl
  where str    = castString x :: String

fBLparse :: Parser String
fBLparse = do
  try (string "" <* eof)
    <|> try (many1 $ oneOf " \r\n\t")
    <|> string "続きを読む"
----------------------------------------------------------------------------------------------------
translateTags :: StringLike a => a -> [TagTree a]
translateTags str = tagTree $ parseTags str

utf8Text :: StringLike a => a -> Text
utf8Text = decodeUtf8 . castString

takeTitle :: MonadReader (Config a) m => a -> m Text
takeTitle tree = do
  ak <- titleAK <$> ask
  f  <- findFunc <$> ask
  return $
    (utf8Text "** " <>) .
    utf8Text . mconcat . map treeText $
    f ak tree

takeText :: MonadReader (Config a) m => a -> m [Text]
takeText tree = do
  ak     <- textAK   <$> ask
  f      <- findFunc <$> ask
  direct <- direct   <$> ask
  return $
    map (<> Tx.pack "\n")          .
    map stringFoldBase             .
    filterBlankLines               .
    concatMap (lines . castString) .
    map (treeTextEx direct)        $
    f ak tree
