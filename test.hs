import Util
import Data.List
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer



a :: State s Int
a = return 1

-- newtype Line a s = Line { runLine :: a -> (s, a) }

-- instance Monad (Line s) where
--   return a = Line $ \s -> (a, s)
--   m >>= k  = Line $ \s -> let
--     (a, s') = runLine m s
--     in runLine (k a) s'

splitComma :: String -> Maybe (String, String)
splitComma str = do
  idx <- findIndex (==',') str
  return $ (take idx str, drop (idx+1) str)

pushCol :: String -> State [String] String
pushCol s = do
  a <- get
  put (s:a)
  return s

test3 :: State String String
test3 = do
  y <- get
  let (x, remain) = takeOneCol y
  put remain
  return x

test4 = do
  x1 <- test3
  x2 <- test3
  _  <- test3
  return [x1, x2]

test2 :: State [Int] Int
test2 = do
  y <- get
  put (2:y)
  return 2

test :: Int -> Writer [String] Int
test i = do
  tell [show i]
  return i
-- test2 :: State a [String]
-- test2 = do
--   a <- get
--   -- let (v, r) = takeOneCol a
--   -- put r
--   return a

-- parseCol :: String -> ([String], String)
-- parseCol s =
  

takeOneCol :: String -> (String, String)
takeOneCol =
  \str -> makeTuple str
  where makeTuple s = case findIndex (==',') s of
          Just x  -> (take x s, drop (x+1) s)
          Nothing -> (s, "")

(-->) a b = \s ->
  let (val, remain) = a s
  in (b remain) s

-- "青木　大助,ｱｵｷ ﾀﾞｲｽｹ,ｱｵｷ ﾀﾞｲｽｹ,男,1979/2/20,37,本,宇治市木幡平尾２８番地の７３６,611-0002,090-52473643,,2015/5/22 8:45,4月17日,1,"
testAlist = [(0, "氏名"), (3, "性別"), (4, "生年月日"), (6, "区分"),
             (11, "日時"), (12, "申込日時"), (13, "無料オプション"), (14, "有料オプション")]

testSplit line =
  map snd $ filter numberExtract cols
  where cols = zip [0..] $ split ',' line
        tal  = map fst testAlist
        numberExtract (n, col) = n `elem` tal
-- test str =
--   takeOneCol str
--   where (-->) a b = let (val, remain) = a
--         in b val
-- popCol :: State s String
-- popCol = do
--   a <- get
--   put $ a ++ "hoge"
--   return a

-- (-->) a b =
--   let (val, remain) = a
--   in b remain

-- parseCol :: State String a
-- parseCol = do
--   a <- get
  

-- hoge :: State String String
-- hoge = do
--   a <- get
--   put ""
--   return a

main :: IO ()
main = do
  test <- map (split '+' . (!!1) . split ',') <$> lines <$> readUTF8File ".test"
  mapM_ putStrLn $ concat test

