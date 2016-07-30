import Util                             (readUTF8line, split, uniq)
import Data.List                        (isInfixOf, sort, sortBy)
import Data.Ratio                       (Ratio (..), (%))
import Data.Array                       (Array (..), listArray, (!))
import Data.Maybe                       (isJust, fromMaybe)
import Data.Monoid                      ((<>))
import Data.Text.Internal               (Text (..))
import Debug.Trace                      (trace)
import Text.Printf                      (printf)
import Text.StringLike                  (castString, StringLike (..))
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import qualified Data.Map               as Map
import qualified System.IO              as I
import qualified Control.Monad.State    as St
import qualified Data.Text              as Tx
import qualified Data.Text.IO           as Txio
import qualified Data.Text.Encoding     as Txe
import qualified Data.ByteString.Char8  as B

type DictLine a   = Array Int a
type Dictionary a = [DictLine a]

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
    | pointHitting h1 < pointHitting h2  = LT
    | pointHitting h1 == pointHitting h2 = EQ
    | otherwise                          = GT

cast :: StringLike a => StringLike b => a -> b
cast = castString

toString :: StringLike a => Answer (a, a) -> String
toString (Absolute (a, b)) = adWithPcode (a, b)
toString (Probably ((a, b), i)) = mconcat ["[" , adWithPcode (a, b), ", ",
                                           show i, "] Probably"]

adWithPcode :: StringLike a => (a, a) -> String
adWithPcode (ad, p) = mconcat [cast ad, " --> ", cast p]

pointHitting :: Hitting -> Ratio Int
pointHitting d = ratio' * (hit & d) + (10 * hitratio d) - (nohit & d) + (10 * continual & d)
  where (&) f d = f d % 1
        -- ratio'  = if "伏見区" `isInfixOf` initial d
        --           then 10
        --           else 1
        ratio'  = 1
----------------------------------------------------------------------------------------------------
makeDict :: IO (Dictionary Text)
makeDict = do
  zips <- readUTF8line ".zipcode.out"
  return $ map toArray zips
  where toArray = listArray (0,1) . split ','
----------------------------------------------------------------------------------------------------
makeKey :: StringLike a => a -> String
makeKey key = case parse kyotoCityP "" (cast key) of
  Right (region, rest) -> region <> rest
  Left _               -> cast key

searchA :: StringLike a => a -> Dictionary Text -> Answer (Dictionary Text)
searchA key dict = searchClassify ordinary verse
  where key'     = cutNumber $ makeKey key
        rev      = Tx.reverse key'
        ordinary = searchCore key' dict
        verse    = searchCore rev dict

(<==>) :: Bool -> (a, a) -> a
(<==>) f (a, b) = if f then a else b

searchClassify :: Dictionary Text -> Dictionary Text -> Answer (Dictionary Text)
searchClassify ord verse = case (ord, verse) of
  ([o], [v]) -> (o == v) <==> (Absolute [o], Probably ([o, v], 2))
  ([o], _)   -> Absolute [o]
  (_, [v])   -> Absolute [v]
  _          -> let ret = uniq $ ord ++ verse in
                Probably (ret, length ret)

cutNumber :: StringLike a => a -> Text
cutNumber = toText . cut . toStr
  where toText key = cast key :: Text
        cut        = takeWhile (`notElem` "0123456789０１２３４５６７８９")
        toStr key  = cast key :: String

searchCore :: Text -> Dictionary Text -> Dictionary Text
searchCore key dict = cut . (`St.execState` dict) $ searchST2 key
  where cut = overLengthAvoid 100

charLookup :: Char -> Map.Map Char Char -> Char
{-# INLINE charLookup #-}
charLookup c m = fromMaybe 'z' (Map.lookup c m)

innerlook :: Char -> DictLine Text -> Bool
{-# INLINE innerlook #-}
innerlook ch line = ch <?> line || charLookup ch otherChar <?> line
  where (<?>) ch line = ch `telem` (line!0)

searchST2 :: Text -> St.State (Dictionary Text) ()
searchST2 t = do
  let len = Tx.length t
  forM_ [0..(len-1)] $ \n -> do
    dic <- St.get
    let ch = Tx.index t n
    case filter (innerlook ch) dic of
      [x]  -> do { St.put [x]; return () }
      []   -> St.put dic
      filt -> St.put filt

overLengthAvoid :: Int -> [a] -> [a]
overLengthAvoid over x = (length x > over) <==> ([], x)

telem :: Char -> Text -> Bool
{-# INLINE telem #-}
telem c tx = isJust (Tx.findIndex (==c) tx)
----------------------------------------------------------------------------------------------------
kyotoCityP :: Parser (String, String)
kyotoCityP = do
  region <- choice [string "北区", string "上京区", string "中京区", string "下京区",
                    string "左京区", string "右京区", string "西京区", string "南区",
                    string "伏見区", string "東山区"]
  _      <- streetP
  rest   <- many anyChar
  return (region, rest)

streetP :: Parser String
streetP = do
  choice $ map try [string "上る", string "下る", string "上ル", string "下ル"]
  <|> (anyChar >> streetP)
----------------------------------------------------------------------------------------------------
addHit :: Char -> Hitting -> Hitting
addHit char hit' =
  hit' { target   = newTarget,
         hit      = newHit,
         hitratio = (newHit - length newTarget) % newHit }
  where newTarget = removeChar char (target hit')
        newHit    = hit hit' + 1

guessHit :: StringLike a => String -> Dictionary a -> (String, String)
guessHit f p = answer . last $ guessHitList key' p
  where key'     = makeKey f
        answer t = (initial t, pcode t)

guessHitList :: StringLike a => String -> Dictionary a -> [Hitting]
guessHitList target dict = sort $ map (givePoint target) dict

givePoint :: StringLike a => String -> DictLine a -> Hitting
givePoint baseStr gen = (`St.execState` initHit) $ do
  forM_ baseStr $ \char -> do
    hit <- St.get
    let newHit = if [char] `isInfixOf` target hit
                 then addHit char hit
                 else hit { nohit = nohit hit + 1 }
    St.put newHit
  hit <- St.get
  St.put $ hit { continual = giveContinualPoint baseStr ad }
  where ad      = cast (gen!0) :: String
        postal  = cast (gen!1) :: String
        initHit = Hitting ad postal ad 0 (0%1) 0 0

giveContinualPoint :: String -> String -> Int
giveContinualPoint key target = sum $ take 2 $ sortBy (flip compare) (map length $ filtering list)
  where filtering = filter (`isInfixOf` target)
        list      = partialList key ++ verseList key

_partialList :: (Int -> String -> String) -> String -> [String]
_partialList f xl = map f [0..(length xl)] <*> [xl]

partialList :: String -> [String]
partialList = _partialList take

verseList :: String -> [String]
verseList = _partialList drop

removeChar :: Char -> String -> String
removeChar char str = loop str []
  where loop (x:xs) r
          | char == x = reverse r ++ xs
          | otherwise = loop xs (x:r)

main :: IO ()
main = do
  dict <- makeDict
  target <- readUTF8line ".test.address" :: IO [Text]
  I.hSetEncoding I.stdout I.utf8
  forM_ target $ \ad -> do
    putStr   $ cast ad ++ ", "
    putStrLn $ Main.toString $ guessHit (cast ad) <$> searchA ad dict

----------------------------------------------------------------------------------------------------
---------- for debug -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
testIO :: String -> IO ()
testIO str = do
  dict <- makeDict
  I.hSetEncoding I.stdout I.utf8
  let ans = searchA str dict
  let toStr ary = mconcat [ary!0, cast " --> ", ary!1]
  case ans of
    Absolute [ary]  -> Txio.putStrLn $ toStr ary
    Probably (a, _) -> mapM_ (Txio.putStrLn . toStr) a

testIO2' :: Text -> St.StateT (Dictionary Text) IO ()
testIO2' key = do
  let len = Tx.length key
  forM_ [0..(len - 1)] $ \n -> do
    dic <- St.get
    let ch   = Tx.index key n
    case filter (innerlook ch) dic of
      [x]  -> do { St.put [x]; return () }
      []   -> St.put dic
      filt -> do
        St.liftIO $ putStr $ show (length filt) ++ " --> "
        St.put filt

testIO2 :: String -> IO ()
testIO2 s = do
  io  <- makeDict
  ary <- (`St.execStateT` io) $ testIO2' (cutNumber $ makeKey s)
  I.hSetEncoding I.stdout I.utf8
  I.putChar '\n'
  mapM_ (Txio.putStrLn . (!0)) ary
