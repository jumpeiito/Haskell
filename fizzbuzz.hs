import System.IO
import Data.List

fact :: Integer -> Integer
fact n = inner n 1
  where
    inner 1 acc = acc
    inner n acc = inner (n - 1) (n * acc)

-- takeLines :: Int -> Handle -> IO [String]
-- takeLines 0 _ = return []
-- takeLines n h = do
--   eof <- hIsEOF h
--   if eof
--      then return []
--     else do
--       x <- hGetLine h
--       xs <- takeLines (n - 1) h
--       return (x:xs)

takeLines :: Int -> Handle -> IO [String]
takeLines 0 _ = return []
takeLines n h = do
  eof <- hIsEOF h
  if eof
     then return []
    else do
      x <- hGetLine h
      xs <- takeLines (n - 1) h
      return (x:xs)


reader :: Handle -> IO [String]
reader h = inner []
  where
    inner r = do
      eof <- hIsEOF h
      if eof
        then do hClose h; return (reverse r)
        else do
        x <- hGetLine h
        inner (x:r)

reader2 :: Handle -> IO [Char]
reader2 h = inner []
  where
    inner r = do
      eof <- hIsEOF h
      if eof
         then do hClose h; return (reverse r)
        else do
        x <- hGetChar h
        inner (x:r)

d = "ほげ"

data CsvChar =
  Comma
  | Newline
  | Quote
  | Return
  | Other Char deriving Show

_cchar :: Char -> CsvChar
_cchar ','  = Comma
_cchar '\n' = Newline
_cchar '\r' = Return
_cchar '"'  = Quote
_cchar c    = Other c

_csvRead :: Handle -> IO [[String]]
_csvRead h = inner [] [] []
  where
    inner cell small r = do
      eof <- hIsEOF h
      if eof
         then return (reverse r)
        else do
        c <- hGetChar h
        case _cchar c of
          Quote   -> inner cell small r
          Return  -> inner cell small r
          Newline -> inner "" [] $ r `rcons` (small `rcons` cell)
          Comma   -> inner "" (small `rcons` cell) r
          Other n -> inner (n:cell) small r
      where
        rcons l1 l2 = (reverse l2) : l1

csvRead :: FilePath -> IO [[String]]
csvRead fp = do
  h <- openFile fp ReadMode
  c <- _csvRead h
  hClose h
  return c

len :: [a] -> Integer
len xs = inner xs 0
  where
    inner [] r = r
    inner (x:xs) c = inner xs (c + 1)

-- avg :: [Int] -> Maybe a
avg [] = Nothing
avg ls = inner ls (0, 0)
  where
    inner [] (r, s) = Just (fromIntegral s / r)
    inner (x:xs) (r, s) = inner xs (r+1, x+s)
         
rotate :: [a] -> [a]
-- rotate [] = []
rotate xs = xs ++ (reverse xs)

rotatep :: Eq a => [a] -> Bool
rotatep [] = False
rotatep [x, y] = if x == y then True else False
rotatep ls =
  if (head ls) == (last ls)
  then rotatep (init $ tail ls)
  else False

sort_core :: [a] -> [a] -> Ordering
sort_core x y
  | length x > length y  = GT
  | length x < length y  = LT
  | length x == length y = EQ

sort2 :: [[a]] -> [[a]]
sort2 xs = sortBy sort_core xs

myIntersperse :: a -> [[a]] -> [a]
myIntersperse y l = inner l []
  where
    inner [] r = reverse r
    inner (x:xs) r =
      case xs of
        [] -> r ++ x
        _  -> inner xs (r ++ (x ++ [y]))

data Tree a =
  Node a (Tree a) (Tree a)
  | Empty
    deriving (Show)

treetest = Node 10 (Node 12 (Node 16 Empty Empty) (Node 10 (Node 12 (Node 14 Empty Empty) Empty) Empty)) Empty

treelen :: Tree a -> Integer
treelen Empty = 0
treelen (Node _ x y) =
  1 + max (treelen x) (treelen y)

testfile = "f:/util2/meibo/特定健診用氏名一覧10北_20131127.csv"
dockfile = "f:/util2/kserv/.dock"

-- docktest [] = ()
-- docktest x:xs = do
--   case x of
--     [_, _, "5615021", _] -> print x
--     otherwise -> ();
--   docktest xs
