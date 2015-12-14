module ZcodeDict where

import Util
import Data.List (sortBy)
import Data.Monoid
import System.IO (hPutStrLn, hSetEncoding, stdout, utf8)
import qualified Data.ByteString.Lazy.Char8 as BC

data Dict a =
  Tree a (Dict a) (Dict a)
  | Leaf a
  | None deriving (Eq, Show, Read)

instance Functor Dict where
  fmap f (Tree a l r) = Tree (f a) (fmap f l) (fmap f r)
  fmap _ None         = None
  fmap f (Leaf s)     = Leaf $ f s

instance Monoid a => Monoid (Either a b) where
  mempty = Left mempty
  Right a `mappend` _ = Right a
  Left _ `mappend` Right a = Right a
  Left _ `mappend` Left _  = Left mempty

initDict :: String -> String -> Dict String
initDict pcode ""     = Leaf pcode
initDict pcode (x:xs) = Tree [x] (initDict pcode xs) None

addDict :: String -> String -> Dict String -> Dict String
addDict _ _ (Leaf _)   = None
addDict pcode "" _     = Leaf pcode
addDict pcode ad None  = initDict pcode ad
addDict pcode ad@(x:xs) (Tree s l r)
  | [x] == s           = Tree s (addDict pcode xs l) r
  | otherwise          = Tree s l (addDict pcode ad r)

-- CSVファイルから辞書TREEを作成
makeDict :: [String] -> Dict String
makeDict l =
  foldl addic None tl
  where ls    = map (split ',') l
        tl    = tail ls
        addic d (a:p:_) = addDict p a d

sortKana :: [String] -> [String]
sortKana = sortBy compare

lineTranslate :: String -> [String]
lineTranslate str = 
  sortKana $ map (\ad -> (reverse ad) ++ "," ++ pcord) addresses
  where addresses  = tail splitted
        pcord      = take 7 $ head splitted
        splitted   = filter (/="") $ split '/' str


-- -- SKK辞書ファイルからCSVファイルを作成
-- skkToCSV :: [String] -> [String]
-- skkToCSV =
--   sortKana . concat . map lineTranslate
-- ------------------------------------------------------------
countLeaves :: Dict a -> Int
countLeaves d =
  loop 0 d
  where loop _ (Leaf _)       = 1
        loop _ None           = 0
        loop c (Tree _ t1 t2) = (loop c t1) + (loop c t2)

-- countPoint :: String -> Dict -> Int
-- countPoint "" _ = 0
-- countPoint str dic =
--   case dic of
--   Leaf _                             -> 0
--   None                               -> 0
--   Tree [a] t1 _ | a `elem` "府県市町" -> countPoint str t1
--                 | a `elem` str       -> (+1) $ countPoint str t1
--                 | otherwise          -> countPoint str t1

returnLeaf :: Dict String -> Dict String
returnLeaf dic =
  case dic of
  None       -> None
  Leaf s     -> Leaf s
  Tree _ l _ -> returnLeaf l

returnLeaves :: Dict String -> [String]
returnLeaves (Tree a l r) = -- undefined
  (map (a++) (returnLeaves l)) ++ (map (a++) (returnLeaves r))
returnLeaves _ = [""]

toString :: Dict String -> String
toString dic =
  case dic of
  None   -> ""
  Leaf _ -> ""
  Tree [a] l _ -> a:(toString l)

------------------------------------------------------------
serialize :: FilePath -> Dict String -> IO ()
serialize oFile dict =
  withOutFile oFile (\h -> hPutStrLn h $ show dict)

deserialize :: FilePath -> IO (Dict String)
deserialize inFile = do
  cont <- readUTF8ByteFile inFile
  return $ read $ BC.unpack cont

deserialize2 :: FilePath -> IO (Dict String)
deserialize2 inFile = do
  cont <- readUTF8File inFile
  return $ read cont

------------------------------------------------------------
runDictBranch :: String -> Dict String -> Either String (Dict String)
runDictBranch xs dic
  | countLeaves dic == 1 = Right $ returnLeaf dic
  | otherwise            = undefined

runDict :: String -> Dict String -> Either String (Dict String)
runDict _ None     = Left "not found"
runDict _ (Leaf l) = Right $ Leaf l
runDict "" dict
  | countLeaves dict == 1 = Right $ returnLeaf dict
  | otherwise             = runDict "区見伏" dict
runDict key@(x:xs) tree@(Tree a l r)
  | [x] == a  = runDict xs l
  | otherwise =
    case r of
    None -> Left "2"
    -- None -> runDictBranch key tree
    -- None -> Right tree
    _    -> runDict key r

searchList :: (Dict String) -> [String]
searchList dict =
  case dict of
  None       -> []
  Leaf _     -> []
  Tree a l r -> (a:(searchList l)) ++ (a:(searchList r))
-- o 宇治市木幡南山1-38
-- o 宇治市小倉町天王40-1レジデンス小倉405号
-- o 城陽市平川東垣外56-1 丸美マンション205
-- o 山科区大塚檀ノ浦19ｸﾞﾗﾝﾄﾞﾒｿﾞﾝ山科312
-- o 宇治市伊勢田町中山42
-- o 城陽市寺田大谷93-8
-- o 宇治市木幡檜尾38-46
-- o 菱屋町677
-- o 舞台町5-1マンション中川305
-- x 山・西野山桜ﾉ馬場町
-- x 宇.六地蔵町並31ｸﾞﾗﾝﾃﾞｲｰﾙﾔﾏﾀﾞ206
-- x 南区大宮八条下ル九条町412-37
-- x 京都市下京区河原町通松原上ル幸竹町３７６ --> 600-8034
-- x 京都市下京区河原町通松原上ル幸竹町３７６ --> 600-8034
search :: (Dict String) -> String -> Either String String
search _ ""    = Left "not found"
search dict key =
  case runDict (reverse key) dict of
  Right (Leaf l) -> Right l
  Left _         -> search dict (init key)

tuplePrint :: (String, Either String String) -> IO ()
tuplePrint (ad, pcode) =
  case pcode of
  Right p  -> putStrLn $ "(" ++ ad ++ ", " ++ p ++ ")"
  Left _   -> putStrLn $ "(" ++ ad ++ ", ---)"

-- winlines :: B.ByteString -> [B.ByteString]
-- winlines str =
--   S.splitOn "\r\n" str

main :: IO ()
main = do
  dict <- deserialize ".dict"
  -- target <- readUTF8File ".test.address"
  target <- readUTF8File ".postal"
  -- let result = map (\key -> (key, search dict key)) $ lines target
  let result = map (search dict) $ lines target
  hSetEncoding stdout utf8
  mapM_ print result
  -- let result = map (search dict) $ lines target
  -- hSetEncoding stdout utf8
  -- mapM_ (putStrLn . show) result

-- test = do
--   a <- get
--   forM_ a 

-- main = do
--   print $ runState test [1..10]
