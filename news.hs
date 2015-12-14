import qualified Network.HTTP as Net
-- import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified System.IO    as I
import qualified Util         as U
import Control.Monad
import Control.Monad.Writer
import Data.Time
import Data.Monoid
import Strdt
import Data.Foldable (foldMap, Foldable)
-- import Codec.Binary.UTF8.String (encodeString)
-- import Control.Applicative
-- import Text.XML.HXT.Core
-- import Text.XML.HXT.XPath
import Text.Regex.Posix
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Control.Applicative
-- import Data.Foldable

-- instance Foldable TagTree where
--  f `foldMap` TagBranch x _ branch = f x `mappend` foldMap f branch
--  f `foldMap` TagLeaf x = f x

instance Monoid a => Monoid (Tag a) where
   mempty = TagText mempty
   TagText x `mappend` TagText y = TagText (x`mappend`y)
   TagText x `mappend` _ = TagText x
   _ `mappend` TagText y = TagText y
   _ `mappend` _ = mempty

instance Monoid a => Monoid (TagTree a) where
   mempty = TagLeaf mempty
   TagBranch x _ trees `mappend` TagBranch y _ trees' =
     (TagLeaf x `mappend` mconcat trees) `mappend` (TagLeaf y `mappend` mconcat trees')
   TagBranch x _ trees `mappend` TagLeaf s =
     (TagLeaf x `mappend` mconcat trees) `mappend` s
   TagLeaf s `mappend` TagBranch x _ trees =
     s `mappend` (TagLeaf x `mappend` mconcat trees)

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
  -- I.hSetEncoding I.stdout I.utf8
  -- mapM_ putStrLn $ lines contents
  contents <- U.readUTF8File "f:/Haskell/1.html"
  I.hSetEncoding I.stdout I.utf8
  let list = extractBlogBody $ translateTags contents
  mapM_ (putStrLn . title) list
  -- mapM_ (putStrLn . show . date) $ list
  -- forM list (mapM_ putStrLn . body)

-- main :: IO ()
-- main = do
--   contents <- getPageContents "http://shasetsu.seesaa.net/category/4848855-1.html"
--   I.hSetEncoding I.stdout I.utf8
--   mapM_ putStrLn $ lines contents
  
