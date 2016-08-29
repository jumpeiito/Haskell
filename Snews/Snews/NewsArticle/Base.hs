module Snews.NewsArticle.Base (ListedPage (..)
                              , URL
                              , ArticleKey
                              , DirectionType
                              , Page (..)
                              , Article (..)
                              , AKey (..)
                              , WriterDirection (..)
                              , findTree
                              , findTreeS
                              , (==>)
                              , findAttribute
                              , findAttributeS
                              , stringFoldBase
                              , treeText
                              , treeTextEx
                              , normalDirection
                              , filterBlankLines
                              , translateTags
                              , getTitle
                              , getText
                              , utf8Text) where

import Util
import Util.StrEnum
import Data.Time                        (Day (..))
import Data.List                        (foldl', isInfixOf)
import Data.Text.Internal               (Text (..))
import Data.Text.Encoding               (decodeUtf8)
import Control.Monad.State              (get, put, State, runState, execState)
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

data ListedPage a =
  LP { baseURL  :: URL,
       topDate  :: Day,
       topURL   :: URL,
       urlF     :: [TagTree a] -> [URL],
       pageF    :: [TagTree a] -> [Page a]
     }

data Page a =
  Page { pageUrl   :: URL,
         tagtree   :: TagTree a,
         titleFunc :: [TagTree a] -> Text,
         textFunc  :: [TagTree a] -> [Text] }

data Article a =
  Article { tree  :: TagTree a,
            title :: a,
            text  :: [a],
            date  :: Maybe Day,
            paper :: a } deriving (Show, Eq)

data AKey = Name String | Attr String | Always deriving (Show, Eq)

data WriterDirection = Skip | Pack String | Loop deriving (Show, Eq)

instance Show (Page a) where
  show (Page u _ _ _) = "Page (url=" ++ u ++ ")"

(==>), findTree :: StringLike a => [ArticleKey] -> TagTree a -> [TagTree a]
findTree akeys tb@TagBranch{} = execWriter $ find' akeys tb
findTree _ _ = []
(==>) = findTree

findTreeS :: StringLike a => [ArticleKey] -> [TagTree a] -> [TagTree a]
findTreeS ak = (findTree ak `concatMap`)

find' :: StringLike a => [ArticleKey] -> TagTree a -> Writer [TagTree a] ()
find' _ (TagLeaf _) = tell mempty
find' akeys tb@(TagBranch _ _ ys)
  | solver akeys tb = tell [tb]
  | otherwise = forM_ ys (tell . findTree akeys)

solver :: StringLike a => [ArticleKey] -> TagTree a -> Bool
solver akeys = Data.List.foldl' (|||) (const False) $ map solveArticleKey akeys

solveArticleKey :: StringLike a => ArticleKey -> TagTree a -> Bool
solveArticleKey (l, r) = solveAKey l &&& solveAKey r

solveAKey :: StringLike a => AKey -> TagTree a -> Bool
solveAKey (Name a) (TagBranch n _ _) = a == castString n
solveAKey (Attr a) (TagBranch _ l _) = [pairF castString ("class", a)] `isInfixOf` l
solveAKey Always _                   = True
solveAKey _ _ = False

pairF :: (t -> t1) -> (t, t) -> (t1, t1)
pairF f (a, b) = (f a, f b)
----------------------------------------------------------------------------------------------------
findAttribute   :: Eq s => s -> TagTree s -> [s]
findAttribute key (TagBranch _ attr tbs) = execWriter fA
  where fA = do
          case assocKey key attr of
            Just y -> tell [y]
            _      -> tell mempty
          forM_ tbs (tell . findAttribute key)
findAttribute _ _ = mempty

findAttributeS :: Eq s => s -> [TagTree s] -> [s]
findAttributeS key = (findAttribute key `concatMap`)

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
  where translate' (n, a, d) = (solveArticleKey (n, a), d)

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
----------------------------------------------------------------------------------------------------
getF :: (Page a -> [TagTree a] -> r) -> Page a -> r
getF f = f <@> (return . tagtree)

(<@>) :: Monad m => m (t -> r) -> m t -> m r
(<@>) f tr = do
  f'  <- f
  tr' <- tr
  return $ f' tr'

getTitle :: Page r -> Text
getText  :: Page r -> [Text]
getTitle = getF titleFunc
getText  = getF textFunc

utf8Text :: StringLike a => a -> Text
utf8Text = decodeUtf8 . castString
