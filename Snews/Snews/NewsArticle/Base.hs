{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Snews.NewsArticle.Base ( URL
                              , ArticleKey
                              , DirectionType
                              , TagElement (..)
                              , Order (..)
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
                              , textFromTree
                              , normalDirection
                              , filterBlankLines
                              , translateTags
                              , utf8Text
                              , takeTitle
                              , takeText) where

-- import Util
import Util.StrEnum
import Data.Time                        (Day (..))
import Data.List                        (foldl', isInfixOf, find)
import Data.Maybe                       (fromMaybe)
import Data.Text.Internal               (Text (..))
import Data.Text.Encoding               (decodeUtf8)
-- import Control.Monad.State              (get, put, State, runState, execState)
import Control.Monad.Reader
import Control.Monad.Writer
import Text.StringLike                  (StringLike, castString)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec                      hiding (State)
import Text.Parsec.String
import qualified Data.Text              as Tx

type URL              = String
type ArticleKey       = (TagElement, TagElement)
type DirectionElement = (TagElement, TagElement, Order)
type Direction        = [DirectionElement]
type DirectionType a  = [(TagTree a -> Bool, Order)]

data TagElement = Name String | Attr String | Always deriving (Show, Eq)

data Order = Skip | Pack String | Loop deriving (Show, Eq)

data Config a = Con { hostName  :: String
                    , baseName  :: String
                    , rootAK    :: [ArticleKey]
                    , titleAK   :: [ArticleKey]
                    , textAK    :: [ArticleKey]
                    , findFunc  :: [ArticleKey] -> a -> [TagTree Text]
                    , direct    :: Direction
                    , urlRecipe :: [URLParts] }
----------------------------------------------------------------------------------------------------
data URLParts =
  Host
  | Base
  | MDay (Day -> String)
  | Str String
  | Slash URLParts

makeURL :: Day -> Reader (Config a) String
makeURL d = ask >>= \Con {..} -> do
  let murl gen (Slash x) = gen ++ murl "" x ++ "/"
      murl gen  Host     = gen ++ hostName
      murl gen  Base     = gen ++ baseName
      murl gen (MDay f)  = gen ++ f d
      murl gen (Str s)   = gen ++ s
  return $ foldl' murl "" urlRecipe
----------------------------------------------------------------------------------------------------
tagName :: TagTree a -> Maybe a
tagName (TagBranch n _ _) = Just n
tagName _ = Nothing

tagAttributes :: TagTree t -> Maybe [Attribute t]
tagAttributes (TagBranch _ attr _) = Just attr
tagAttributes _ = Nothing

tagDescends :: TagTree t -> Maybe [TagTree t]
tagDescends (TagBranch _ _ desc) = Just desc
tagDescends _ = Nothing

findTree :: StringLike a => [ArticleKey] -> TagTree a -> [TagTree a]
findTree akeys tb@TagBranch{} = execWriter $ find' akeys tb
  where
    find' _ (TagLeaf _)   = tell mempty
    find' akeys tb@(TagBranch _ _ ys)
      | matchTree akeys tb = tell [tb]
      | otherwise          = forM_ ys (tell . findTree akeys)
findTree _ _ = []

findTreeS, (<~) :: StringLike a => [ArticleKey] -> [TagTree a] -> [TagTree a]
findTreeS ak = (findTree ak `concatMap`)
(<~) = findTreeS

infixr 9 <~
infixr 9 <~~

(<&&>), (<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) f1 f2 = getAll . ((All . f1) <> (All . f2))
(<||>) f1 f2 = getAny . ((Any . f1) <> (Any . f2))

matchTree :: StringLike a => [ArticleKey] -> TagTree a -> Bool
matchTree akeys =
  foldl' (<||>) (const False) $ map logicProduct akeys
  where
    logicProduct (l, r) = matchAKey l <&&> matchAKey r

matchAKey :: StringLike a => TagElement -> TagTree a -> Bool
matchAKey (Name a) (TagBranch n _ _) = a == castString n
matchAKey (Attr a) (TagBranch _ l _) = [pairF castString ("class", a)] `isInfixOf` l
matchAKey Always _                   = True
matchAKey _ _                        = False

pairF :: (t -> t1) -> (t, t) -> (t1, t1)
pairF f (a, b) = (f a, f b)
----------------------------------------------------------------------------------------------------
findAttribute :: (Eq s, StringLike s) => s -> TagTree s -> [s]
findAttribute key (TagBranch _ attr tbs) = execWriter fA
  where fA = do
          case assocKey (castString key) attr of
            Just y -> tell [y]
            _      -> tell mempty
          forM_ tbs (tell . findAttribute key)
findAttribute _ _ = mempty

findAttributeS, (<~~) :: (Eq s, StringLike s) => s -> [TagTree s] -> [s]
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
treeText    = textFromTree normalDirection
----------
normalDirection :: Direction
normalDirection =
  [(Name "script", Always,          Skip),
   (Name "div",    Attr "posted",   Skip),
   (Name "div",    Attr "bookmark", Skip),
   (Name "p",      Attr "date",     Skip),
   (Name "br",     Always,          Pack "\n"),
   (Always,        Always,          Loop)]

directionElementMatch ::
  StringLike a => DirectionElement -> TagTree a -> Bool
directionElementMatch (name, attr, _) tb =
  matchAKey name tb && matchAKey attr tb

toOrder :: DirectionElement -> Order
toOrder (_, _, o) = o

direction :: StringLike a => Direction -> TagTree a -> Order
direction direct tb =
  fromMaybe Skip $ toOrder <$> find (`directionElementMatch` tb) direct

----------------------------------------------------------------------------------------------------
textFromTree :: (StringLike a, Monoid a) => Direction -> TagTree a -> a
textFromTree dl = execWriter . textFromTreeWriter dl
  where
    textFromTreeWriter _ (TagLeaf (TagText s)) = tell s
    textFromTreeWriter dl tb@(TagBranch _ _ descend) =
      case dl `direction` tb of
        Skip   -> tell mempty
        Pack n -> tell $ castString n
        Loop   -> forM_ descend (tell . textFromTree dl)
    textFromTreeWriter _ _ = tell mempty
----------------------------------------------------------------------------------------------------
strip :: StringLike a => a -> Text
strip = tailCut . skip . (<> Tx.pack "\n") . decode
  where decode  = decodeUtf8 . castString
        skip    = Tx.dropWhile (`elem` [' ', '\t', '\12288', '\n'])
        tailCut = Tx.reverse . skip . Tx.reverse

filterBlankLines :: (StringLike a, Monoid a) => [a] -> [Text]
filterBlankLines [] = []
filterBlankLines (x:xl) =
  case parse parser "" (castString x) of
    Right _ -> filterBlankLines xl
    Left _  -> strip x : filterBlankLines xl
  where
    parser :: Parser String
    parser = do
      try (string "" <* eof)
        <|> try (many1 $ oneOf " \r\n\t")
        <|> string "続きを読む"
----------------------------------------------------------------------------------------------------
translateTags :: StringLike a => a -> [TagTree a]
translateTags str = tagTree $ parseTags str

utf8Text :: StringLike a => a -> Text
utf8Text = decodeUtf8 . castString

takeTitle :: MonadReader (Config a) m => a -> m Text
takeTitle tree = ask >>= \Con {..} -> do
  return $
    (utf8Text "** " <>) .
    utf8Text . mconcat . map treeText $
    findFunc titleAK tree

takeText :: MonadReader (Config a) m => a -> m [Text]
takeText tree = ask >>= \Con {..} -> do
  return $
    map (<> Tx.pack "\n")          .
    map stringFoldBase             .
    filterBlankLines               .
    concatMap (lines . castString) .
    map (textFromTree direct)      $
    findFunc textAK tree
