module NewsArticle (Article (..), always, AKey (..), ListedPage (..),
                    Page (..), makeAkahataPage, makeArticle, treeText, findTree, (<@>),
                    titleFO, output, takePaper,
                    makeAkahata) where

import Strdt
import Data.Time
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Text          as Tx
import qualified Data.Text.Encoding as Txe
import qualified Data.Text.Internal as Txi
import qualified Data.Text.IO       as Txio
import qualified System.IO          as I
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Writer
-- import Control.Arrow
import Data.ByteString.Char8 as B  hiding (map, count, reverse, isInfixOf, concatMap, foldl, elem)
import System.Process
import qualified Control.Monad.State as St
import qualified Network.HTTP as Net
import Data.Text.ICU.Convert as C

type URL = String

data ListedPage a b = LP { baseURL   :: URL,
                           topDate   :: Day, 
                           topURL    :: URL,
                           listedKey :: [TagTree a] -> [URL] }

-- "http://www.jcp.or.jp/akahata/"
makeAkahata :: Day -> ListedPage ByteString Day
makeAkahata d = LP base' d url' (akahataNewsList base')
  where base' = "http://www.jcp.or.jp/akahata/"
        url'  = akahataMakeURL d base'

akahataNewsList :: URL -> [TagTree ByteString] -> [URL]
akahataNewsList base = map (akahataF base) . extractHref . extractTree
  where extractTree = (findTree (Always, Attr "newslist") `concatMap`)
        extractHref = (findAttribute (B.pack "href")      `concatMap`)

akahataF :: URL -> ByteString -> URL
akahataF base url = akahataURL d base url'
  where url' = unpack url
        dstr = Data.List.take 8 url'
        d    = fromJust $ strdt dstr

akahataMakeURL :: Day -> URL -> URL
akahataMakeURL day base = akahataURL day base "index.html"

akahataURL :: Day -> URL -> URL -> URL
akahataURL day base subpage =
  base <> "aik" <> nendo' </> day' </> subpage
  where (</>) a = (a <>) . ("/" <>)
        nendo'  = show $ (`mod` 1000) $ nendo day
        day'    = dayStrWithSep '-' day

data Page a = Page { pageUrl   :: URL,
                     titleFunc :: [TagTree a] -> a,
                     textFunc  :: [TagTree a] -> [Txi.Text] }

data Article a = Article { tree  :: TagTree a,
                           title :: a,
                           text  :: [a],
                           date  :: Maybe Day,
                           paper :: a }
               deriving (Show, Eq)

takeAkahataTitle :: [TagTree ByteString] -> ByteString
takeAkahataTitle tr = orgStar <> treeTextMap tree'
  where tree'   = ((Name "title", Always) ==>) `concatMap` tr
        orgStar = pack "** "

takeAkahataText :: [TagTree ByteString] -> [Txi.Text]
takeAkahataText = map stringFold . filter' . treeToStringList . makeTree
  where makeTree = concatMap $ findTrees [(Name "p",  Always),
                                          -- (Name "h1", Always),
                                          -- (Name "h2", Always),
                                          (Name "h3", Always)]
        treeToStringList = map treeText
        filter' = filterBlankLines

makeAkahataPage :: URL -> Page ByteString
makeAkahataPage url = Page url takeAkahataTitle takeAkahataText

data AKey = Name String | Attr String | Always deriving (Show, Eq)

type ArticleKey = (AKey, AKey)
-- "http://www.jcp.or.jp/akahata/aik16/2016-07-13/2016071301_01_1.html"

(<@>) :: (String, String) -> [(ByteString, ByteString)] -> Bool
(a, b) <@> alist = [pairF pack (a,b)] `isInfixOf` alist

----------------------------------------------------------------------------------------------------
always          :: a -> Bool
solveAKey       :: AKey -> TagTree ByteString -> Bool
solveArticleKey :: ArticleKey -> TagTree ByteString -> Bool
solver          :: [ArticleKey] -> TagTree ByteString -> Bool
(&&&)           :: Monad m => m Bool -> m Bool -> m Bool
(|||)           :: Monad m => m Bool -> m Bool -> m Bool
(==>), findTree :: ArticleKey -> TagTree ByteString -> [TagTree ByteString]
find'           :: ArticleKey -> TagTree ByteString -> Writer [TagTree ByteString] ()
assocKey        :: Eq a => a -> [(a, b)] -> Maybe b
findAttribute   :: Eq s => s -> TagTree s -> [s]

always _ = True

solveAKey (Name a) (TagBranch n _ _) = n == pack a
solveAKey (Attr a) (TagBranch _ l _) = [pairF pack ("class", a)] `isInfixOf` l
solveAKey Always _                   = always ()
solveAKey _ _ = False
        
(&&&) x y = do
  f1 <- x
  f2 <- y
  return $ f1 && f2

(|||) x y = do
  f1 <- x
  f2 <- y
  return $ f1 || f2

solveArticleKey (l, r) = solveAKey l &&& solveAKey r

solver akeys = Data.List.foldl' (|||) (const False) $ map solveArticleKey akeys

findTree akey tb@(TagBranch {}) = execWriter $ find' akey tb
findTree _ _ = []
(==>) = findTree

findTrees akeys tb@(TagBranch {}) = execWriter $ finds' akeys tb
findTrees _ _ = []

find' akey tb@(TagBranch _ _ ys)
  | solveArticleKey akey tb = tell [tb]
  | otherwise = St.forM_ ys (tell . findTree akey)
find' _ _ = tell []

finds' akeys tb@(TagBranch _ _ ys)
  | solver akeys tb = tell [tb]
  | otherwise = St.forM_ ys (tell . findTrees akeys)

assocKey _ [] = Nothing
assocKey k ((x, y):rest)
  | k == x = Just y
  | otherwise = assocKey k rest

findAttribute key (TagBranch _ attr tbs) = execWriter fA
  where fA = do
          case assocKey key attr of
            Just y -> tell [y]
            _ -> tell []
          forM_ tbs (tell . findAttribute key)
          return ()
findAttribute _ _ = []
  
----------------------------------------------------------------------------------------------------
treeText    :: TagTree ByteString -> ByteString
treeTextMap :: [TagTree ByteString] -> ByteString
makeArticle :: TagTree ByteString -> Article ByteString
takeTitle   :: TagTree ByteString -> ByteString
takeText    :: TagTree ByteString -> [ByteString]
titleFO     :: Article ByteString -> ByteString
textFO      :: Article ByteString -> ByteString
output      :: Article ByteString -> ByteString
----------
pairF f (a, b) = (f a, f b)
----------
-- treeAttribute attrKey (TagBranch _ attr _) =
----------
treeText s      = execWriter $ ttx s 
treeTextMap     = mconcat . map treeText
treeText2 dl s  = execWriter $ ttx s
treeTextMap2 dl = mconcat . map (treeText2 dl)
----------
ttx :: TagTree ByteString -> Writer ByteString ()
ttx (TagLeaf (TagText s)) = tell s
ttx tb@(TagBranch _ _ descend) =
  case direction tb directionList of
  Skip   -> tell mempty
  Pack n -> tell $ pack n
  Loop   -> St.forM_ descend (tell . treeText)
ttx _ = tell mempty

data WriterDirection = Skip | Pack String | Loop deriving (Show, Eq)

type DirectionType = [(TagTree ByteString -> Bool, WriterDirection)]

dList :: [(AKey, AKey, WriterDirection)]
dList = 
  [(Name "script", Always,          Skip),
   (Name "div",    Attr "posted",   Skip),
   (Name "div",    Attr "bookmark", Skip),
   (Name "a",      Always,          Skip),
   (Name "p",      Attr "date",     Skip),
   (Name "br",     Always,          Pack "\n"),
   (Always,        Always,          Loop)]

directionList :: DirectionType
directionList = map translate' dList
  where translate' (n, a, d) = (solveArticleKey (n, a), d)

direction _ [] = Skip
direction tb ((f, direct):xs)
  | f tb = direct
  | otherwise = direction tb xs

----------
makeArticle tagtree =
  Article { tree  = tagtree,
            title = takeTitle tagtree,
            text  = takeText tagtree,
            date  = getDate $ takeTitle tagtree,
            paper = takePaper $ takeTitle tagtree }

-- makearticleakahata tagtree =
--   article { tree  = tagtree,
--             title = takeakahatatitle tagtree,
--             text  = takeakahatatext tagtree,
--             date  = getakahatadate tagtree,
--              paper = b.pack "赤旗"
--           }
-------------
takeTitle        = treeTextMap . findTree (Always, Attr "title")

-- ----------
titleFO a = pack "** " <> title a <> B.pack "\n"
textFO a  = B.intercalate (B.pack "\n") $ map (pack "   " <>) $ text a
output    = titleFO <> textFO
------------
takeText  = filterBlankLines . treeToStringList . makeTree
  where makeTree = findTree (Always, Attr "text")
        treeToStringList = B.lines . treeTextMap

strip :: ByteString -> ByteString
strip = B.dropWhile (`elem` [' ', '\NUL', '\t'])

filterBlankLines :: [ByteString] -> [ByteString]
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
getDate :: ByteString -> Maybe Day
getDate s = case parse getDateParse "" s of
  Right x -> strdt (B.unpack x)
  Left _  -> Nothing

getAkahataDate :: TagTree ByteString -> Maybe Day
getAkahataDate tree = strdt (B.unpack date')
  where date' = treeTextMap $ (Always, Attr "date") ==> tree

dateP :: Parser ByteString
dateP = do
  year <- char '(' *> count 4 digit <* anyChar
  mon  <- count 2 digit <* anyChar
  day  <- count 2 digit <* anyChar <* char ')'
  return $ B.pack $ year ++ "/" ++ mon ++ "/" ++ day

getDateParse :: Parser ByteString
getDateParse = try dateP <|> (anyChar >> getDateParse)
----------------------------------------------------------------------------------------------------
takePaper :: ByteString -> ByteString
takePaper s = case parse takePaperParse mempty s of
  Right s' -> s'
  Left _   -> mempty

takePaperParse :: Parser ByteString
takePaperParse = try inner <|> (anyChar >> takePaperParse)
  where inner = do
        char '['
        rel <- many1 (noneOf "]")
        char ']'
        return $ B.pack rel
----------------------------------------------------------------------------------------------------
testStr = B.pack wn
  where wn = [ 'あ' | x <- [1..1000] ]

stringFold :: ByteString -> Txi.Text
stringFold s = (Tx.pack "   ") <> sfold s' 0
  where s' = Txe.decodeUtf8 s

sfold :: Txi.Text -> Int -> Txi.Text
sfold tx c
  | tx == mempty = mempty
  | otherwise = let Just (ch, rest) = Tx.uncons tx in
    let char' = Tx.pack [ch] in
    if c == 35
    then char' <> Tx.pack "\n   " <> sfold rest 0
    else char' <> sfold rest (c+1)
----------------------------------------------------------------------------------------------------
translateTags :: ByteString -> [TagTree ByteString]
translateTags str = tagTree $ parseTags str
-- ----------------------------------------------------------------------------------------------------
extractBlogBody :: [TagTree ByteString] -> [TagTree ByteString]
extractBlogBody =
  concatMap $ findTree (Always, Attr "blogbody")

extractHtml :: [TagTree ByteString] -> [TagTree ByteString]
extractHtml = concatMap $ findTree (Name "html", Always)
-------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
testIO = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./4.html"] Nothing Nothing
  page <- B.hGetContents p
  let contents = map makeArticle $ extractBlogBody $ translateTags page
  mapM_ (mapM_ B.putStrLn . text) contents

-- testIO2 = do
--   (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./5.html"] Nothing Nothing
--   page <- B.hGetContents p
--   return $ concatMap (findAttribute (B.pack "href")) $ akahataNewsList $ translateTags page

testIO21 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./5.html"] Nothing Nothing
  page <- B.hGetContents p
  return $ akahataNewsList "http://www.jcp.or.jp/akahata/" $ translateTags page

testIO22 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./6.html"] Nothing Nothing
  page <- B.hGetContents p
  return $ translateTags page

testIO3 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./6.html"] Nothing Nothing
  page <- translateTags <$> B.hGetContents p
  let akpage = makeAkahataPage ""
  B.putStrLn $ titleFunc akpage page
  I.hSetEncoding I.stdout I.utf8
  mapM_ Txio.putStrLn $ textFunc akpage page

testStr1 = "　\tこうした北朝鮮の核・ミサイル開発が深刻な脅威となっているとみて、米韓は配備を最終決定した。2017年末までの運用開始をめざすという。不測の事態に備えた迎撃態勢の強化とともに、北朝鮮の核・ミサイル開発を抑制する効果も期待できるだろう。"

testStr2 = "最近は投票率の低下が目立つ。衆院選は2012年、14年と２回続けて過去最低を更新。13年の前回参院選は選挙区で52.61％と歴代３位の低さだった。有権者の半数が棄権する状況は民主主義の土台を揺るがしかねず、どこかで流れを変えなければならない。"

testStr3 = "今回は選挙権年齢が「20歳以上」から「18歳以上」に引き下げられた。投票所への子どもの同伴も全面解禁になった。若い層が積極的に投票すれば、高齢者の影響力が大きい「シルバー民主主義」の流れを変えられる。家族で選挙に行って子や孫に投票する姿を見せるのは、将来の有権者への何より身近な主権者教育となる。"

numaddOver10 :: [Int] -> Int
-- numaddOver10 = (Data.List.filter odd) >>> (Data.List.filter (>10)) >>> Data.List.length
numaddOver10 = Data.List.length . Data.List.filter (>10) . Data.List.filter odd

testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "[読売新聞] 震災遺構の(保存)　合意形成へ議論を尽くそう (2015年08月24日)"), TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]]

 
