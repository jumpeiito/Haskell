import qualified Network.HTTP as Net
-- import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified System.IO    as I
import qualified Util         as U
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State as St
import Data.Time
import Data.Monoid
import Strdt
import Data.Foldable (foldMap, Foldable)
import Text.Regex.Posix
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Control.Applicative

data BlogBody = BlogBody { title   :: String,
                           body    :: [String],
                           bbtree  :: [TagTree String],
                           date    :: Maybe Day
                         } deriving Show

getPageContents :: String -> IO String
getPageContents url = 
  (Net.simpleHTTP $ Net.getRequest url) >>= Net.getResponseBody

find2 :: Eq a => Eq b => a -> b -> [(a,b)] -> Bool
find2 key val alist =
  case lookup key alist of
  Just v -> v == val
  Nothing -> False

translateTags :: String -> [TagTree String]
translateTags str = tagTree $ parseTags str

extractBranch :: String -> String -> String -> [TagTree String] -> [[TagTree String]]
extractBranch n key val =
  foldl extract [] 
  where extract r (TagBranch name attrs subtree)
          | name == n && find2 key val attrs = subtree:r
          | otherwise = r ++ extractBranch n key val subtree
        extract r _ = r

extractTitle :: [TagTree String] -> String
extractTitle taglist =
  tex
  where list = extractBranch "a" "class" "title" taglist
        [[TagLeaf (TagText tex)]] = list

extractDate :: String -> Maybe Day
extractDate = strdt . reverse . take 11 . drop 1 . reverse

insertNewLines :: Int -> String -> String
insertNewLines n str =
  inLoop str 1 []
  where inLoop (x:xs) c r =
          if c >= n && x `notElem` ['\12289', '\12290']
          then inLoop xs 1 ('\n':x:r)
          else inLoop xs (c+1) (x:r)
        inLoop [] _ r = reverse r

extractText :: [TagTree String] -> [String]
extractText taglist =
  reverse $ foldl foldText [] list
  where list = concat $ extractBranch "div" "class" "text" taglist
        foldText r (TagLeaf (TagText s)) = (insertNewLines 35 s):r
        foldText r _ = r

bodyLine :: [String] -> [String]
bodyLine =
  map $ unlines . map ("  "++) . lines
  
extractBlogBody :: [TagTree String] -> [BlogBody]
extractBlogBody taglist =
  map makebb list
  where list = extractBranch "div" "class" "blogbody" taglist
        makebb tree = BlogBody { title   = extractTitle tree,
                                 body    = bodyLine $ extractText tree,
                                 bbtree  = tree,
                                 date    = extractDate $ extractTitle tree
                               }

-- main :: IO [()]
main = do
  -- contents <- getPageContents "http://shasetsu.seesaa.net/category/4848855-1.html"
  -- I.hSetEncoding I.stdout I.utf8xo
  -- mapM_ putStrLn $ lines contents
  contents <- U.readUTF8File "f:/Haskell/1.html"
  I.hSetEncoding I.stdout I.utf8
  let list = extractBlogBody $ translateTags contents
  mapM_ (putStrLn . title) list
  -- mapM_ (putStrLn . show . date) $ list
  -- forM list (mapM_ putStrLn . body)

testIO = do
  contents <- U.readUTF8File "f:/Haskell/1.html"
  I.hSetEncoding I.stdout I.utf8
  mapM_ (putStrLn . treeText) $ concatMap (findTree at (find2 "class" "text")) $ translateTags contents

at :: n -> Bool
at _ = True

findTree :: (String -> Bool) -> ([Attribute String] -> Bool) -> TagTree String -> [TagTree String]
findTree hF aF tb@(TagBranch _ _ _) =
  (`execState` []) $ _findT hF aF tb
findTree _ _ _ = []

_findT :: (String -> Bool) -> ([Attribute String] -> Bool) -> TagTree String -> St.State [TagTree String] ()
_findT hF aF tb@(TagBranch header attr y)
  | (hF header) && (aF attr) = do { St.modify (tb:); return ()} 
  | otherwise = do
      St.forM y $ \n -> St.modify ((findTree hF aF n)++)
      return ()
_findT _ _ _ = return ()

treeText :: TagTree String -> String
treeText s = (`execState` "") $ _tTxt s

_tTxt :: TagTree String -> St.State String ()
_tTxt (TagBranch "script" _ _) = return ()
-- _tTxt (TagLeaf (TagText "\n")) = return ()
_tTxt (TagBranch _ _ ts)       = do { St.forM ts $ \t -> St.modify (++ (treeText t)); return () }
_tTxt (TagLeaf (TagText s))    = do { St.modify (++ s); return ()}
_tTxt _ = return ()

testfoo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "foo"),TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")],TagBranch "h3" [("class","title")] [TagLeaf (TagText "buz")]]
                                
foo = TagBranch "div" [("class","blogbody")] [TagLeaf (TagText "\n"),TagBranch "h3" [("class","title")] [TagBranch "a" [("href","http://shasetsu.seesaa.net/article/424759504.html"),("class","title")] [TagLeaf (TagText "[\35501\22770\26032\32862] \21335\21271\39640\23448\21332\35696\12288\34909\31361\12398\22238\36991\12408\20919\38745\12395\27497\12415\23492\12428 (2015\24180\&08\26376\&25\26085)")]],TagLeaf (TagText "\n"),TagBranch "div" [("class","text")] [TagLeaf (TagText "\36557\20107\30340\25361\30330\12391\32202\24373\12434\39640\12417\12388\12388\12289\23550\35441\12391\35698\27497\12434\24341\12365\20986\12377\12371\12392\12434\29401\12358\8212\8212\12290\21271\26397\39854\12399\12289\21361\38522\12394\28716\25144\38555\25126\34899\12434\33258\21046\12377\12409\12365\12384\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\38867\22269\12392\21271\26397\39854\12398\36557\20107\30340\32202\24373\12398\39640\12414\12426\12434\21463\12369\12390\12289\21335\21271\39640\23448\21332\35696\12364\26495\38272\24215\12391\38283\20652\12373\12428\12383\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\38867\22269\12399\37329\23515\37806\22823\32113\38936\24220\22269\23478\23433\20445\23460\38263\12425\12364\12289\21271\26397\39854\12399\37329\27491\24681\31532\65297\26360\35352\12398\20596\36817\12398\40644\28851\29790\12539\26397\39854\20154\27665\36557\32207\25919\27835\23616\38263\12425\12364\20986\24109\12375\12383\12290\30064\20363\12398\39640\12356\12524\12505\12523\12398\23550\35441\12399\65299\26085\36899\32154\12391\34892\12431\12428\12383\12364\12289\38627\33322\12375\12390\12356\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\36557\20107\34909\31361\12392\12356\12358\26368\24746\12398\20107\24907\12434\36991\12369\12427\12383\12417\12289\21452\26041\12399\12289\31309\26997\30340\12395\27497\12415\23492\12426\12289\32202\24373\32233\21644\12398\20855\20307\30340\12394\25514\32622\12434\25506\12427\12371\12392\12364\27714\12417\12425\12428\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\32202\24373\12398\30330\31471\12399\12289\65300\26085\12395\36557\20107\22659\30028\32218\20184\36817\12391\12289\21271\26397\39854\12364\35373\32622\12375\12383\12392\12415\12425\12428\12427\22320\38647\12364\29190\30330\12375\12289\38867\22269\36557\20853\22763\65298\20154\12364\37325\20663\12434\36000\12387\12383\12371\12392\12384\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\38867\22269\20596\12399\12289\20241\25126\21332\23450\36949\21453\12384\12392\21271\26397\39854\12434\38750\38627\12375\12383\12290\23550\25239\25514\32622\12392\12375\12390\12289\21271\26397\39854\20307\21046\12434\25209\21028\12377\12427\25313\22768\27231\12398\23459\20253\25918\36865\12434\65297\65297\24180\12406\12426\12395\20877\38283\12375\12383\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\21271\26397\39854\12399\12289\25918\36865\20013\27490\12434\35201\27714\12375\12289\38867\22269\20869\12395\30770\25731\12375\12383\12290\12373\12425\12394\12427\36557\20107\34892\21205\12434\20104\21578\12375\12383\12358\12360\12289\38867\22269\20596\12395\21332\35696\12434\30003\12375\20837\12428\12383\12290\21271\26397\39854\12395\24540\25126\12375\12383\38867\22269\12398\26420\27135\24693\25919\27177\12395\23550\12377\12427\30828\36575\20001\27096\12398\25594\12373\12406\12426\12391\12354\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\12371\12398\32972\26223\12395\12399\12289\21271\26397\39854\12398\22269\38555\30340\12394\23396\31435\12364\25351\25688\12373\12428\12427\12290\26680\23455\39443\12398\24375\34892\12394\12393\12395\12424\12426\12289\26368\22823\12398\21451\22909\22269\12289\20013\22269\12392\12398\38306\20418\12364\20919\12360\36796\12415\12289\39318\33075\32026\20132\27969\12399\36884\32118\12360\12390\12356\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\20013\22269\12398\25239\26085\25126\21213\35352\24565\34892\20107\12408\12398\26420\22823\32113\38936\20986\24109\12364\30330\34920\12373\12428\12383\30452\24460\12395\21271\26397\39854\12364\30770\25731\12375\12383\12398\12399\12289\20013\38867\25509\36817\12434\29309\21046\65288\12369\12435\12379\12356\65289\12377\12427\29401\12356\12384\12429\12358\12363\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\21069\32218\37096\38538\12395\12300\28310\25126\26178\29366\24907\12301\12434\30330\20196\12375\12383\12398\12399\12289\37329\27491\24681\31532\65297\26360\35352\12384\12290\22269\20869\20307\21046\12364\19981\23433\23450\12394\20013\12289\25351\23566\32773\12398\32076\39443\12395\20047\12375\12356\37329\31532\65297\26360\35352\12364\12289\19968\35302\21363\30330\12398\29366\27841\12395\36969\20999\12394\23550\20966\12364\12391\12365\12427\12398\12363\12290\19981\23433\12399\25325\12360\12394\12356\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\19968\26041\12289\26420\27663\12399\12300\25361\30330\12395\12399\26029\22266\12392\12375\12390\23550\24540\12377\12427\12301\12392\20844\35328\12377\12427\12290\25919\27177\25903\25345\29575\12399\20302\36855\12375\12289\38867\22269\12513\12487\12451\12450\12399\21271\26397\39854\12395\23550\12377\12427\24375\30828\12394\23039\21218\12434\20027\24373\12375\12390\12356\12427\12290\26420\27663\12418\12289\23481\26131\12395\22949\21332\12391\12365\12394\12356\22269\20869\20107\24773\12434\25265\12360\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\12384\12364\12289\21335\21271\21452\26041\12399\12289\36557\20107\34909\31361\12398\12522\12473\12463\12434\30452\35222\12375\12289\20919\38745\12395\23550\24540\12377\12427\12371\12392\12364\32925\35201\12391\12354\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\21271\26397\39854\25351\23566\37096\12399\12289\38867\22269\12398\8220\33029\23041\8221\12434\24375\35519\12377\12427\12371\12392\12391\12289\36557\12420\26397\39854\21172\20685\20826\12394\12393\22269\20869\12398\24341\12365\32224\12417\12434\22259\12387\12390\12356\12427\12392\12373\12428\12427\12290\65297\65296\26376\65297\65296\26085\12398\20826\21109\24314\65303\65296\21608\24180\12395\21512\12431\12379\12390\12289\22269\23041\30330\25562\12398\30446\30340\12391\12289\20013\38263\36317\38626\24382\36947\12511\12469\12452\12523\12434\30330\23556\12377\12427\12392\12398\35251\28204\12418\12354\12427\12290"),TagBranch "br" [] [],TagBranch "br" [] [],TagLeaf (TagText "\26085\31859\38867\65299\12363\22269\12399\12289\32202\23494\12395\36899\25658\12375\12289\21271\26397\39854\24773\21218\12398\24773\22577\20849\26377\12434\24375\21270\12377\12427\24517\35201\12364\12354\12427\12290\26032\12383\12394\36557\20107\30340\25361\30330\12434\23553\12376\36796\12417\12427\12383\12417\12289\25233\27490\12395\19975\20840\12434\26399\12377\12371\12392\12418\22823\20999\12384\12290"),TagBranch "div" [("id","article-ad"),("style","margin-top:20px; text-align:center;clear:boxbth;")] [TagLeaf (TagText "\n"),TagBranch "script" [("type","text/javascript")] [TagLeaf (TagText "<!--\nseesaa_bg_color     = 'FFFFFF';\nseesaa_title_color  = '939393';\nseesaa_text_color   = '666666';\nseesaa_template_id  = '4001';\nseesaa_hostsite_id  = '1';\nseesaa_site_id      = 'shasetsu';\nseesaa_genre_id     = '0';\nseesaa_article_id   = '424759504';\nseesaa_keyword_char = 'utf8';\nseesaa_keyword_list = '';\nseesaa_adcount      = '3';\nseesaa_image_ad     = true;\n//-->")],TagLeaf (TagText "\n"),TagBranch "script" [("type","text/javascript"),("src","http://match.seesaa.jp/-/js/square_under_seesaa.js")] [],TagLeaf (TagText "\n")],TagLeaf (TagText "\n\n \n\n"),TagBranch "div" [("class","bookmark"),("data-url","http://shasetsu.seesaa.net/article/424759504.html"),("data-subject","[\35501\22770\26032\32862] \21335\21271\39640\23448\21332\35696\12288\34909\31361\12398\22238\36991\12408\20919\38745\12395\27497\12415\23492\12428 (2015\24180\&08\26376\&25\26085)")] [],TagLeaf (TagText "\n\n\n")],TagLeaf (TagText "\n"),TagBranch "div" [("class","posted")] [TagLeaf (TagText "posted by \65288-@\8704@\65289 at 11:20| "),TagBranch "a" [("href","http://shasetsu.seesaa.net/article/424759504.html#comment")] [TagLeaf (TagText "Comment(0)")],TagLeaf (TagText "\n\n| "),TagBranch "a" [("href","http://shasetsu.seesaa.net/article/424759504.html#trackback")] [TagLeaf (TagText "TrackBack(0)")],TagLeaf (TagText "\n| "),TagBranch "a" [("href","http://shasetsu.seesaa.net/category/4848855-1.html")] [TagLeaf (TagText "\35501\22770\26032\32862")],TagLeaf (TagText "\n| "),TagBranch "a" [("href","http://blog.seesaa.jp/cms/fan/regist/input?fan_blog_name=shasetsu"),("title","\12371\12398\12502\12525\12464\12398\35501\32773\12395\12394\12427")] [TagBranch "img" [("src","http://blog.seesaa.jp/img/fan_read.gif"),("alt","\12371\12398\12502\12525\12464\12398\35501\32773\12395\12394\12427"),("border","0")] []],TagLeaf (TagText "\n| "),TagBranch "a" [("href","http://blog.seesaa.jp/cms/home/switch?goto=/cms/fan/blog_reader/list%3Fblog_id=1571266"),("title","\26356\26032\24773\22577\12434\12481\12455\12483\12463\12377\12427")] [TagBranch "img" [("src","http://blog.seesaa.jp/img/fan_received.gif"),("alt","\26356\26032\24773\22577\12434\12481\12455\12483\12463\12377\12427"),("border","0")] []],TagLeaf (TagText "\n")],TagLeaf (TagText "\n\n")]
