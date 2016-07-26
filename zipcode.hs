import Util
import Data.List
import Data.Char
import Data.Ratio
import Text.Printf
import Control.Monad.State              as St
import Text.Regex.Posix
import Control.Monad
import Codec.Binary.UTF8.String
import GHC.IO.Handle.Types
import System.Environment (getArgs)
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, hClose, mkTextEncoding, stdout, utf8, hPutStrLn, hFlush)
import qualified Data.ByteString.Char8  as B

data Dict =
  Tree String Dict Dict
  | Leaf String 
  | None deriving (Eq, Show, Read)

initDict :: String -> String -> Dict
initDict pcode "" = Leaf pcode
initDict pcode (x:xs) = Tree [x] (initDict pcode xs) None

addDict :: String -> String -> Dict -> Dict
addDict pcode "" _ = Leaf pcode
addDict pcode ads None = initDict pcode ads
addDict pcode ads@(x:xs) (Tree a l r)
  | [x] == a    = Tree a (addDict pcode xs l) r
  | otherwise   = Tree a l (addDict pcode ads r)
addDict pcode ads@(x:xs) (Leaf _)  = None
  
sortKana :: [String] -> [String]
sortKana = sortBy (\a b -> compare a b)

lineTranslate :: String -> [String]
lineTranslate str = 
  sortKana $ map (\ad -> (reverse ad) ++ "," ++ pcord) addresses
  where addresses = tail splitted
        pcord     = take 7 $ head splitted
        splitted  = filter (not . isBlank) $ split '/' str
        isBlank s = s == ""

lineToCSV :: String -> [String]
lineToCSV =
  sortKana . concat . map lineTranslate . lines

csvPrint :: Handle -> String -> IO ()
csvPrint h strs = 
  mapM_ (hPutStrLn h) $ lineToCSV strs

lineToDict :: [String] -> Dict
lineToDict l =
  foldl addic None tl
  where ls    = map (split ',') l
        tl    = tail ls
        addic d (a:p:_) = addDict p a d

countLeaves :: Dict -> Int
countLeaves d =
  loop 0 d
  where loop c d' =
          case d' of
          Leaf _ -> 1
          None   -> 0
          Tree _ t1 t2 -> (loop c t1) + (loop c t2)

isSingleton :: Dict -> Bool
isSingleton d =
  case d of
  (Leaf _) -> True
  None -> True
  Tree _ a None -> isSingleton a
  Tree _ _ _ -> False

returnLeaf :: Dict -> Maybe String
returnLeaf (Leaf s) = Just s
returnLeaf None = Nothing
returnLeaf (Tree _ l _) = returnLeaf l

runDictDict :: String -> Dict -> Maybe Dict
runDictDict key d =
  look key' d
  where key' = reverse key
        look _ None = Nothing
        look _ (Leaf s) = Just (Leaf s)
        look "" t = Just t
        look k@(x:xs) (Tree a l r)
          | [x] == a  = look xs l
          | otherwise = look k r

runDict :: String -> Dict -> Maybe String
runDict key d =
  look key' d
  where key' = reverse key
        look _ None = Nothing
        look _ (Leaf s) = Just s
        look "" _ = Nothing
        look k@(x:xs) (Tree a l r)
          | [x] == a && isSingleton l = returnLeaf l
          | [x] == a                  = look xs l
          | otherwise                 = look k r

serializeDict :: FilePath -> Dict -> IO ()
serializeDict oFile dict =
  withOutFile oFile (\h -> hPutStrLn h $ show dict)

deserializeDict :: FilePath -> IO Dict
deserializeDict inFile = do
  cont <- readUTF8File inFile
  return $ read cont

toCSV :: FilePath -> FilePath -> IO ()
toCSV inFile outFile = do
  contents <- readUTF8File inFile
  withOutFile outFile (\handle -> csvPrint handle contents)

extractCore :: String -> String
extractCore "" = ""
extractCore [x] =
  if isDigit x then "" else [x]
extractCore (x:y) =
  if isDigit $ head y then [x] else x:(extractCore y)
  
search :: Dict -> String -> (String, Maybe String)
search _ "" = ("", Nothing)
search dict key =
  (key, loop $ extractCore key)
  where loop "" = Nothing
        loop k' =
          case runDict k' dict of
          Nothing -> loop $ init k'
          Just x  -> Just x

tuplePrint :: (String, Maybe Int) -> IO ()
tuplePrint (ad, pcode) =
  case pcode of
  Just p  -> putStrLn $ "(" ++ ad ++ ", " ++ show p ++ ")"
  Nothing -> putStrLn $ "(" ++ ad ++ ", ---)"

main :: IO ()
main = do
  dict <- testIO-- deserializeDict ".dict"
  target <- readUTF8File ".test.address"
  let result    = map (\n -> (n, Just $ length $ search' n dict)) $ lines target
  hSetEncoding stdout utf8
  mapM_ tuplePrint result

-- testIO :: IO ()
testIO = do
  zips <- readUTF8File ".zipcode.out"
  let dic = map (split ',') $ lines zips
  return dic

testIO2 = do
  zips <- B.pack <$> readUTF8File ".zipcode.out"
  let dic = map (split ',') $ B.lines zips
  return dic

search' :: String -> [[String]] -> [[String]]
search' target dic =
  St.execState (testSearch target) dic

testSearch :: String -> State [[String]] ()
testSearch xs = do
  forM_ xs $ \x -> do
    dic <- St.get
    let filt = filter (\line -> x `elem` head line) dic
    case dic of
      [_] -> St.put dic
      _   -> do
        if null filt
          then St.put dic
          else St.put filt

testl = map reverse
  [["滋賀県大津市比叡平", ""],
   ["滋賀県大津市大平", ""],
   ["兵庫県姫路市大津区平松", ""],
   ["滋賀県大津市平津", ""],
   ["滋賀県大津市膳所平尾町", ""],
   ["滋賀県大津市上田上平野町", ""],
   ["滋賀県大津市平野", ""]]

testl2 = map reverse
  [["大阪府摂津市南千里丘", ""],
   ["大阪府摂津市千里丘", ""],
   ["滋賀県大津市一里山", ""],
   ["大阪府摂津市千里丘東", ""],
   ["滋賀県大津市仰木の里東", ""],
   ["滋賀県大津市仰木の里", ""],
   ["滋賀県大津市池の里", ""],
   ["滋賀県大津市鶴の里", ""],
   ["滋賀県大津市里", ""],
   ["滋賀県大津市滋賀里", ""]]

-- givePoint :: String -> String -> (String, Int, Int)
-- givePoint x target
--   | x `isInfixOf` target = (target, length x, 0)
--   | otherwise = givePoint (reverse $ tail $ reverse x) target
-- givePoint :: String -> St.State [(String, Int)] ()

data Hitting = Hitting { initial   :: String,
                         target    :: String,
                         hit       :: Int,
                         hitratio  :: Ratio Int,
                         nohit     :: Int,
                         continual :: Int
                       } deriving (Show, Eq)

pointHitting :: Hitting -> Ratio Int
pointHitting d = (hit & d) + (10 * hitratio d) - (nohit & d) + (10 * continual & d)
  where (&) f d = (f d) % 1

instance Ord Hitting where
  compare h1@(Hitting {}) h2@(Hitting {})
    | pointHitting h1 < pointHitting h2  = LT
    | pointHitting h1 == pointHitting h2 = EQ
    | otherwise            = GT

addHit char hit' =
  Hitting { initial  = initial hit',
            target   = newTarget,
            hit      = newHit,
            hitratio = (newHit - (length newTarget)) % newHit,
            nohit    = nohit hit',
            continual = continual hit'
          }
  where newTarget = removeChar char (target hit')
        newHit    = (hit hit') + 1

guessHit :: String -> [[String]] -> String
guessHit target dict = initial $ top $ sort hitter
  where hitter = map (givePoint target) $ map head dict
        top    = head . reverse

givePoint :: String -> String -> Hitting
givePoint baseStr gen = (`St.execState` initHit) $ do
  forM_ baseStr $ \char -> do
    hit <- St.get
    let newHit = if ([char] `isInfixOf` (target hit))
                 then addHit char hit
                 else hit { nohit = nohit hit + 1 }
    St.put newHit
  hit <- St.get
  St.put $ hit { continual = giveContinualPoint baseStr gen }
  where initHit = Hitting gen gen 0 (0%1) 0 0

-- guessHit :: String -> [[String]] -> 
giveContinualPoint :: String -> String -> Int
giveContinualPoint "" _ = 0
giveContinualPoint bsx@(b:bs) gen
  | bsx `isInfixOf` gen = length bsx
  | otherwise = giveContinualPoint bs gen
  
removeChar :: Char -> String -> String
removeChar char str = loop str []
  where loop (x:xs) r
          | char == x = reverse r ++ xs
          | otherwise = loop xs (x:r)

-- search'' :: B.ByteString -> [[B.ByteString]] -> [[B.ByteString]]
-- search'' target dic =
--   St.execState (testSearch' target) dic

-- testSearch' :: B.ByteString -> State [[B.ByteString]] ()
-- testSearch' xs = do
--   forM_ xs $ \x -> do
--     dic <- St.get
--     let filt = filter (\line -> x `elem` head line) dic
--     case dic of
--       [_] -> St.put dic
--       _   -> do
--         if null filt
--           then St.put dic
--           else St.put filt
-- data Filtering a = Filt $ \n -> b n

-- instance Functor Filtering where
--   f `fmap` F x = F $ x . (map f)

-- instance Monad Filtering where
--   return a = F { runFilt = filter (const True) }

-- instance Applicative Filtering where
--   pure = return

-- instance Functor Filtering where
--   f `fmap` F x = F $ \xl -> map f $ x xl
