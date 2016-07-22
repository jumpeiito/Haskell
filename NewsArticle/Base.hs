module NewsArticle.Base (ListedPage (..),
                         URL, ArticleKey, DirectionType,
                         Page (..),
                         Article (..),
                         AKey (..),
                         WriterDirection (..),
                         findTree,
                         (==>),
                         findAttribute,
                         stringFold,
                         treeText,
                         treeTextEx,
                         normalDirection,
                         filterBlankLines,
                         translateTags,
                         getTitle,
                         getText) where

import Data.Time
import Data.List
import Control.Monad.Writer
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.Text             as Tx
import qualified Data.Text.Internal    as Txi
import qualified Data.Text.Encoding    as Txe
import qualified Data.Text.IO    as Txio
import qualified System.IO    as I
import qualified Data.ByteString.Char8 as B

type URL           = String
type ArticleKey    = (AKey, AKey)
type DirectionList = [(AKey, AKey, WriterDirection)]
type DirectionType = [(TagTree B.ByteString -> Bool, WriterDirection)]

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
         titleFunc :: [TagTree a] -> a,
         textFunc  :: [TagTree a] -> [Txi.Text] }

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

(==>), findTree :: [ArticleKey] -> TagTree B.ByteString -> [TagTree B.ByteString]
findTree akeys tb@(TagBranch {}) = execWriter $ find' akeys tb
findTree _ _ = []
(==>) = findTree

find' :: [ArticleKey] -> TagTree B.ByteString -> Writer [TagTree B.ByteString] ()
find' akeys tb@(TagBranch _ _ ys)
  | solver akeys tb = tell [tb]
  | otherwise = forM_ ys (tell . findTree akeys)

solver :: [ArticleKey] -> TagTree B.ByteString -> Bool
solver akeys = Data.List.foldl' (|||) (const False) $ map solveArticleKey akeys

(&&&), (|||) :: Monad m => m Bool -> m Bool -> m Bool
(&&&) x y = do
  f1 <- x
  f2 <- y
  return $ f1 && f2

(|||) x y = do
  f1 <- x
  f2 <- y
  return $ f1 || f2

solveArticleKey :: ArticleKey -> TagTree B.ByteString -> Bool
solveArticleKey (l, r) = solveAKey l &&& solveAKey r

solveAKey :: AKey -> TagTree B.ByteString -> Bool
solveAKey (Name a) (TagBranch n _ _) = n == B.pack a
solveAKey (Attr a) (TagBranch _ l _) = [pairF B.pack ("class", a)] `isInfixOf` l
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
            _ -> tell []
          forM_ tbs (tell . findAttribute key)
          return ()
findAttribute _ _ = []

assocKey :: Eq a => a -> [(a, b)] -> Maybe b
assocKey _ [] = Nothing
assocKey k ((x, y):rest)
  | k == x = Just y
  | otherwise = assocKey k rest
----------------------------------------------------------------------------------------------------
stringFold :: Txi.Text -> Txi.Text
stringFold s = (Tx.pack "   ") <> sfold s 0

sfold :: Txi.Text -> Int -> Txi.Text
sfold tx c
  | tx == mempty = mempty
  | otherwise = let Just (ch, rest) = Tx.uncons tx in
    let char' = Tx.pack [ch] in
    if c == 33
    then char' <> Tx.pack "\n   " <> sfold rest 0
    else char' <> sfold rest (c+1)
----------------------------------------------------------------------------------------------------
treeText :: TagTree B.ByteString -> B.ByteString
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

directionTranslate :: DirectionList -> DirectionType
directionTranslate = map translate'
  where translate' (n, a, d) = (solveArticleKey (n, a), d)

directionList :: DirectionType
directionList = directionTranslate normalDirection

direction _ [] = Skip
direction tb ((f, direct):xs)
  | f tb = direct
  | otherwise = direction tb xs
----------------------------------------------------------------------------------------------------
treeTextEx :: DirectionList -> TagTree B.ByteString -> B.ByteString
treeTextEx dl = execWriter . ttxex dl

ttxex :: DirectionList -> TagTree B.ByteString -> Writer B.ByteString ()
ttxex _ (TagLeaf (TagText s)) = tell s
ttxex dl tb@(TagBranch _ _ descend) =
  case direction tb dx of
    Skip   -> tell mempty
    Pack n -> tell $ B.pack n
    Loop   -> forM_ descend (tell . treeTextEx dl)
  where dx = directionTranslate dl
ttxex _ _ = tell mempty    
----------------------------------------------------------------------------------------------------
strip :: B.ByteString -> Txi.Text
strip = tailCut . skip . (<> Tx.pack "\n") . decode
  where decode  = Txe.decodeUtf8
        skip    = Tx.dropWhile (`elem` [' ', '\t', '\12288', '\n'])
        tailCut = Tx.reverse . skip . Tx.reverse

filterBlankLines :: [B.ByteString] -> [Txi.Text]
filterBlankLines [] = []
filterBlankLines (x:xl) = case parse fBLparse "" x of
  Right _ -> filterBlankLines xl
  Left _  -> strip x : filterBlankLines xl

fBLparse :: Parser String
fBLparse = do
  try (string "" <* eof)
    <|> try (many1 $ oneOf " \r\n\t")
    <|> string "続きを読む"
  return mempty
----------------------------------------------------------------------------------------------------
translateTags :: B.ByteString -> [TagTree B.ByteString]
translateTags str = tagTree $ parseTags str
----------------------------------------------------------------------------------------------------
getF :: (Page a -> [TagTree a] -> r) -> Page a -> r
getF f = f <@> (return . tagtree)

(<@>) :: Monad m => m (t -> r) -> m t -> m r
(<@>) f tr = do
  f'  <- f
  tr' <- tr
  return $ f' tr'

getTitle :: Page r -> r
getText  :: Page r -> [Txi.Text]
getTitle = getF titleFunc
getText  = getF textFunc
