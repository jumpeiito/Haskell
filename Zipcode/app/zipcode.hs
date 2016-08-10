module Main where

import           Util                   (readUTF8line, split, uniq, include)
import           ZipDist
import           ZipFormatta            (fmtFold)
import           Data.List              (isInfixOf, sort, sortBy)
import           Data.Ratio             (Ratio, (%))
import           Data.Array             (Array, listArray, (!))
import           Data.Maybe             (isJust, fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text.Internal     (Text (..))
import           Text.StringLike        (castString, StringLike (..))
import           Text.Parsec            hiding (State)
import           Text.Parsec.String
import           Control.Monad
import           Control.Monad.State    (execState, put, get, State)
import           Control.Parallel.Strategies
import qualified Data.Map               as Map
import qualified System.IO              as I
import qualified Data.Text              as Tx

{-# INLINE charLookup #-}
{-# INLINE innerlook #-}
{-# INLINE telem #-}

type DictLine a   = Array Int a
type Dictionary   = [DictLine Text]

otherChar :: Map.Map Char Char
otherChar = Map.fromList [('\12534', '\12465'),
                          ('\12465', '\12534'),
                          ('\28149', '\28181'),
                          ('\28181', '\28149')]
  
data Answer a = Absolute a | Probably (a, Int)
                deriving (Show, Eq)

data Hitting = Hitting { initial   :: String,
                         pcode     :: String,
                         target    :: String,
                         hit       :: Int,
                         hitratio  :: Ratio Int,
                         nohit     :: Int,
                         continual :: Int
                       } deriving (Show, Eq)

instance Functor Answer where
  f `fmap` Absolute a = Absolute $ f a
  f `fmap` Probably (a, b) = Probably (f a, b)

instance Ord Hitting where
  compare h1@(Hitting {}) h2@(Hitting {})
    | point h1 < point h2  = LT
    | point h1 == point h2 = EQ
    | otherwise            = GT

point :: Hitting -> Ratio Int
point d = ratio' * (hit & d) + (10 * hitratio d) - (nohit & d) + (10 * continual & d)
  where (&) f d' = f d' % 1
        ratio'   = if "伏見区" `isInfixOf` initial d
                   then 2
                   else 1
----------------------------------------------------------------------------------------------------
cast :: StringLike a => StringLike b => a -> b
cast = castString

toString :: StringLike a => Answer (a, a) -> String
toString (Absolute (a, b)) =      fmtFold a b "{ad} --> {pt}"
toString (Probably ((a, b), i)) = (fmtFold a b "[{ad} --> {pt}], Probably ") ++ show i
----------------------------------------------------------------------------------------------------
makeDict :: IO Dictionary
makeDict = map (listArray (0,1) . split ',')
           <$> readUTF8line "f:/Haskell/Zipcode/.zipcode.out"

makeDistrictDict :: District -> IO Dictionary
makeDistrictDict district = 
  filter (include dis . Tx.unpack . (!0)) <$> makeDict
  where dis = let District add lis = district
              in map (add++) lis
  
----------------------------------------------------------------------------------------------------
makeKey :: StringLike a => a -> String
makeKey key = case parse kyotoCityP "" (cast key) of
  Right (region, rest) -> region <> rest
  Left _               -> cast key

searchA :: StringLike a => a -> Dictionary -> Answer Dictionary
searchA key dict = searchClassify o v
  where key'   = cutNumber $ makeKey key
        rev'   = Tx.reverse key'
        (o, v) = runEval $ do
          ord   <- rpar $ searchCore key' dict
          verse <- rpar $ searchCore rev' dict
          rseq ord
          rseq verse
          return $ (ord, verse)

(<==>) :: Bool -> (a, a) -> a
(<==>) f (a, b) = if f then a else b

searchClassify :: Dictionary -> Dictionary -> Answer Dictionary
searchClassify ord verse = case (ord, verse) of
  ([o], [v]) -> (o == v) <==> (Absolute [o], Probably ([o, v], 2))
  ([o], _)   -> Absolute [o]
  (_, [v])   -> Absolute [v]
  _          -> let ret = uniq $ ord ++ verse
                in Probably (ret, length ret)

cutNumber :: StringLike a => a -> Text
cutNumber = toText . cut . toStr
  where toText key = cast key :: Text
        cut        = takeWhile (`notElem` "0123456789０１２３４５６７８９")
        toStr key  = cast key :: String

searchCore :: Text -> Dictionary -> Dictionary
searchCore key dict = cut . (`execState` dict) $ searchST2 key
  where cut = overLengthAvoid 100

charLookup :: Char -> Map.Map Char Char -> Char
charLookup c m = fromMaybe 'z' (Map.lookup c m)

innerlook :: Char -> DictLine Text -> Bool
innerlook ch line = ch <<?>> line ||
                    charLookup ch otherChar <<?>> line
  where (<<?>>) c l = c `telem` (l!0)

searchST2 :: Text -> State Dictionary ()
searchST2 t = do
  let len = Tx.length t
  forM_ [0..(len-1)] $ \n -> do
    dic <- get
    let ch = Tx.index t n
    case filter (innerlook ch) dic of
      [x]  -> do { put [x]; return () }
      []   -> put dic
      filt -> put filt

overLengthAvoid :: Int -> [a] -> [a]
overLengthAvoid over x = (length x > over) <==> ([], x)

telem :: Char -> Text -> Bool
telem c tx = isJust (Tx.findIndex (==c) tx)
----------------------------------------------------------------------------------------------------
kyotoCityP :: Parser (String, String)
kyotoCityP = 
  (,) <$> choice (map string $ fromDistrict kyotoDistrict)
      <*> (streetP *> many anyChar)

streetP :: Parser String
streetP = do
  choice $ map try [string "上る", string "下る", string "上ル", string "下ル"]
  <|> (anyChar >> streetP)
----------------------------------------------------------------------------------------------------
addHit :: Char -> Hitting -> Hitting
addHit ch hit' =
  hit' { target   = newTarget,
         hit      = newHit,
         hitratio = (newHit - length newTarget) % newHit }
  where newTarget = removeChar ch (target hit')
        newHit    = hit hit' + 1

guessHit :: String -> Dictionary -> (String, String)
guessHit f p = answer . last $ guessHitList key' p
  where key'     = makeKey f
        answer t = (initial t, pcode t)

guessHitList :: String -> Dictionary -> [Hitting]
guessHitList trgt dict = sort $ map (givePoint trgt) dict

givePoint :: StringLike a => String -> DictLine a -> Hitting
givePoint baseStr gen = (`execState` initHit) $ do
  hit' <- get
  put $ hit' { continual = giveContinualPoint baseStr ad }
  forM_ baseStr $ \ch -> do
    h <- get
    put ([ch] `isInfixOf` target h <==> (addHit ch h,
                                            h { nohit = nohit h + 1 }))
  where ad      = cast (gen!0) :: String
        postal  = cast (gen!1) :: String
        initHit = Hitting ad postal ad 0 (0%1) 0 0

giveContinualPoint :: String -> String -> Int
giveContinualPoint key trgt = sum $ take 2 $ sortBy (flip compare) (map length $ filtering list)
  where filtering = filter (`isInfixOf` trgt)
        list      = partialList key ++ verseList key

_partialList :: (Int -> String -> String) -> String -> [String]
_partialList f xl = map f [0..(length xl)] <*> [xl]

partialList :: String -> [String]
partialList = _partialList take

verseList :: String -> [String]
verseList = _partialList drop

removeChar :: Char -> String -> String
removeChar ch str = loop str []
  where loop [] _ = mempty
        loop (x:xs) r
          | ch == x = reverse r ++ xs
          | otherwise = loop xs (x:r)

casePair :: Monoid a => [(Bool, a)] -> a
casePair [] = mempty
casePair (p:ps) = if bool then second else casePair ps
  where (bool, second) = p

main :: IO ()
main = do
  dict <- makeDict
  dic2 <- makeDistrictDict fushimiDistrict
  dic3 <- makeDistrictDict kyotoDistrict
  dic4 <- makeDistrictDict shigaDistrict
  dic5 <- makeDistrictDict ujiDistrict
  dic6 <- makeDistrictDict yamashinaDistrict
  --------------------------------------------------
  trgt <- readUTF8line "f:/Haskell/Zipcode/.test.address" :: IO [Text]
  I.hSetEncoding I.stdout I.utf8
  forM_ trgt $ \ad -> do
    let ad' = Tx.unpack ad
    let dic = casePair [(fromDistrict fushimiDistrict   `include` ad', dic2),
                        (fromDistrict kyotoDistrict     `include` ad', dic3),
                        (fromDistrict shigaDistrict     `include` ad', dic4),
                        (fromDistrict ujiDistrict       `include` ad', dic5),
                        (fromDistrict yamashinaDistrict `include` ad', dic6),
                        (True,                                         dict)]
    putStr   $ ad' ++ ", "
    putStrLn $ Main.toString $ guessHit (cast ad) <$> searchA ad dic

----------------------------------------------------------------------------------------------------
---------- for debug -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
test1 key = do
  dict <- makeDict
  return $ searchCore (Tx.pack key) dict ++ searchCore (Tx.pack (reverse key)) dict

test2 key = do
  dict <- makeDict
  return . runEval $ do
    a <- rpar $ searchCore (Tx.pack key) dict
    b <- rpar $ searchCore (Tx.pack (reverse key)) dict
    rseq a
    rseq b
    return $ a ++ b
