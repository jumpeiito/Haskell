module NewsArticle (Article (..), always, AKey (..), ListedPage (..),
                    makeArticle, treeText, findTree, (<@>),
                    titleFO, output, takePaper,
                    akahataNewsList, akahataMakeURL, topPageURL) where

import Strdt
import Data.Time
import Data.List
import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Writer
import Control.Arrow
import Data.ByteString.Char8 as B  hiding (map, count,reverse, isInfixOf, concatMap, foldl, elem)
import System.Process
import qualified Control.Monad.State as St
import qualified Network.HTTP as Net
import Data.Text.ICU.Convert as C

type URL = String

data ListedPage a b = LP { baseURL   :: URL,
                           urlFunc   :: b -> URL -> URL,
                           listedKey :: [TagTree a] -> [a] }


-- "http://www.jcp.or.jp/akahata/"
akahataBaseURL :: String
akahataBaseURL = "http://www.jcp.or.jp/akahata/aik"

akahataFullURL :: ByteString -> ByteString
akahataFullURL url = pack (akahataBaseURL <> nendo' </> day' <> "/") <> url
  where (</>) a b  = a <> "/" <> b
        dayString  = Data.List.take 8 (unpack url)
        dayDay     = fromJust (strdt dayString :: Maybe Day)
        nendo'     = show $ nendo dayDay `mod` 1000
        day'       = dayStrWithSep '-' dayDay

akahataNewsList :: [TagTree ByteString] -> [ByteString]
akahataNewsList = (map akahataFullURL) . extractHref . extractTree
  where extractTree = concatMap (findTree (Always, Attr "newslist"))
        extractHref = concatMap (findAttribute (B.pack "href"))

akahataMakeURL :: Day -> URL -> URL
akahataMakeURL day base =
  base <> "aik" <> nendo' </> day' </> "index.html"
  where (</>) a = (a <>) . ("/" <>)
        nendo'  = show $ (`mod` 1000) $ nendo day
        day'    = dayStrWithSep '-' day

topPageURL :: b -> ListedPage a b -> URL
topPageURL b xl = (urlFunc xl) b (baseURL xl)

data Page a = Page { pageUrl :: String,
                     textkey :: ArticleKey
                   }

data Article a = Article { tree  :: TagTree a,
                           title :: a,
                           text  :: [a],
                           date  :: Maybe Day,
                           paper :: a }
               deriving (Show, Eq)

data AKey = Name String | Attr String | Always deriving (Show, Eq)

type ArticleKey = (AKey, AKey)
-- "http://www.jcp.or.jp/akahata/aik16/2016-07-13/2016071301_01_1.html"

(<@>) :: (String, String) -> [(ByteString, ByteString)] -> Bool
(a, b) <@> alist = [pairF pack (a,b)] `isInfixOf` alist

----------------------------------------------------------------------------------------------------
always :: a -> Bool
always _ = True

solveAKey :: AKey -> (TagTree ByteString -> Bool)
solveAKey (Name a) (TagBranch n _ _) = n == pack a
solveAKey (Attr a) (TagBranch _ l _) = [pairF pack ("class", a)] `isInfixOf` l
solveAKey Always _                   = always ()
solveAKey _ _ = False
        
solveArticleKey :: ArticleKey -> (TagTree ByteString -> Bool)
solveArticleKey  (l, r) = do
  left'  <- solveAKey l
  right' <- solveAKey r
  return $ left' && right'

findTree akey tb@(TagBranch {}) = execWriter $ find' akey tb
findTree _ _ = []
(==>) = findTree

find' akey tb@(TagBranch header attr ys)
  | solveArticleKey akey tb = tell [tb]
  | otherwise = St.forM_ ys (tell . findTree akey)
find' _ _ = tell []

assocKey    :: Eq a => a -> [(a, b)] -> Maybe b
assocKey _ [] = Nothing
assocKey k ((x, y):rest)
  | k == x = Just y
  | otherwise = assocKey k rest

findAttribute :: Eq s => s -> TagTree s -> [s]
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
tTxt        :: TagTree ByteString -> Writer ByteString ()
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
treeText s = execWriter $ tTxt s
treeTextMap = mconcat . map treeText
----------
tTxt (TagLeaf (TagText s)) = tell s
tTxt (TagBranch name attr tr)
  | name == pack "script" ||
    name == pack "div" && ("class", "posted")   <@> attr ||
    name == pack "div" && ("class", "bookmark") <@> attr
                 = tell mempty
  | name == pack "br" = tell $ pack "\n"
  | otherwise    = St.forM_ tr (tell . treeText)
tTxt _ = tell mempty
----------
makeArticle tagtree =
  Article { tree  = tagtree,
            title = takeTitle tagtree,
            text  = takeText tagtree,
            date  = getDate $ takeTitle tagtree,
            paper = takePaper $ takeTitle tagtree }

makeArticleAkahata tagtree =
  Article { tree  = tagtree,
            title = takeAkahataTitle tagtree,
            text  = takeAkahataText tagtree,
            date  = getAkahataDate tagtree,
             paper = B.pack "赤旗"
          }
-------------
takeTitle        = treeTextMap . findTree (Always, Attr "title")
takeAkahataTitle = treeTextMap . findTree (Name "title", Always)
-- ----------
titleFO a = pack "** " <> title a <> B.pack "\n"
textFO a  = B.intercalate (B.pack "\n") $ map (pack "   " <>) $ text a
output    = titleFO <> textFO
------------
takeText  = filterBlankLines . treeToStringList . makeTree
  where makeTree = findTree (Always, Attr "text")
        treeToStringList = B.lines . treeTextMap

takeAkahataText :: TagTree ByteString -> [ByteString]
takeAkahataText = treeToStringList . makeTree
  where makeTree = findTree (Name "p", Always)
        treeToStringList = map treeText

filterBlankLines :: [ByteString] -> [ByteString]
filterBlankLines [] = []
filterBlankLines (x:xl) = case parse fBLparse "" x of
  Right _ -> filterBlankLines xl
  Left _  -> x : filterBlankLines xl

fBLparse :: Parser ByteString
fBLparse = do { try (many1 $ oneOf " \r\n\t") <|> string "続きを読む";
                return mempty }
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
  where wn = [ '9' | x <- [1..1000] ]
stringFold :: ByteString -> ByteString
stringFold s = sfold2 s 0 

sfold2 :: ByteString -> Int -> ByteString
sfold2 bs c
  | bs == mempty = mempty
  | otherwise = let Just (ch, rest) = B.uncons bs in
    if c == 104
    then B.singleton ch <> B.pack "\n" <> sfold2 rest 0
    else B.singleton ch <> sfold2 rest (c+1)
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
  return $ akahataNewsList $ translateTags page

testIO22 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./5.html"] Nothing Nothing
  page <- B.hGetContents p
  return $ translateTags page

testIO3 = do
  (_, p, _, _) <- runInteractiveProcess "f:/tools/cat.exe" ["./6.html"] Nothing Nothing
  page <- B.hGetContents p
  let contents = extractHtml $ translateTags page
  let akahata  = map makeArticleAkahata contents
  mapM_ (B.putStrLn . titleFO) akahata
  mapM_ (mapM_ B.putStrLn . text) akahata

testStr1 = "こうした北朝鮮の核・ミサイル開発が深刻な脅威となっているとみて、米韓は配備を最終決定した。2017年末までの運用開始をめざすという。不測の事態に備えた迎撃態勢の強化とともに、北朝鮮の核・ミサイル開発を抑制する効果も期待できるだろう。"

testStr2 = "最近は投票率の低下が目立つ。衆院選は2012年、14年と２回続けて過去最低を更新。13年の前回参院選は選挙区で52.61％と歴代３位の低さだった。有権者の半数が棄権する状況は民主主義の土台を揺るがしかねず、どこかで流れを変えなければならない。"

testStr3 = "今回は選挙権年齢が「20歳以上」から「18歳以上」に引き下げられた。投票所への子どもの同伴も全面解禁になった。若い層が積極的に投票すれば、高齢者の影響力が大きい「シルバー民主主義」の流れを変えられる。家族で選挙に行って子や孫に投票する姿を見せるのは、将来の有権者への何より身近な主権者教育となる。"

numaddOver10 :: [Int] -> Int
-- numaddOver10 = (Data.List.filter odd) >>> (Data.List.filter (>10)) >>> Data.List.length
numaddOver10 = Data.List.length . Data.List.filter (>10) . Data.List.filter odd

testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "[読売新聞] 震災遺構の(保存)　合意形成へ議論を尽くそう (2015年08月24日)"), TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]]
