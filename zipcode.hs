import Util
import Data.List                        (isInfixOf, sort)
import Data.Ratio                       (Ratio (..), (%))
import Data.Array                       (Array (..), listArray, (!))
import Data.Maybe
import Data.Text.Internal               (Text (..))
import Debug.Trace                      (trace)
import Text.Printf                      (printf)
import Control.Monad
import qualified System.IO              as I
import qualified Control.Monad.State    as St
import qualified Data.Text              as Tx
import qualified Data.Text.IO           as Txio
import qualified Data.Text.Encoding     as Txe
import qualified Data.ByteString.Char8  as B
import qualified Text.StringLike        as Like

type DictLine a   = Array Int a
type Dictionary a = [DictLine a]

data Answer a = Absolute a
  | Probably (a, Int)
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
  f `fmap` Probably (a, b) = Probably $ (f a, b)

instance Ord Hitting where
  compare h1@(Hitting {}) h2@(Hitting {})
    | pointHitting h1 < pointHitting h2  = LT
    | pointHitting h1 == pointHitting h2 = EQ
    | otherwise                          = GT

cast :: Like.StringLike a => Like.StringLike b => a -> b
cast = Like.castString

toString :: Like.StringLike a => Answer (a, a) -> String
toString (Absolute (a, b)) = adWithPcode (a, b)
toString (Probably ((a, b), i)) = "[" ++ adWithPcode (a, b) ++ ", " ++ show i ++ "]"

adWithPcode :: Like.StringLike a => (a, a) -> String
adWithPcode (ad, p) = mconcat [cast ad, " --> ", cast p]

pointHitting :: Hitting -> Ratio Int
pointHitting d = ratio' * (hit & d) + (10 * hitratio d) - (nohit & d) + (10 * continual & d)
  where (&) f d = (f d) % 1
        -- ratio'  = if "伏見区" `isInfixOf` initial d
        --           then 10
        --           else 1
        ratio'  = 1
----------------------------------------------------------------------------------------------------
-- makeDict :: (ReadFile a, Like.StringLike a, Splittable a) => IO (Dictionary a)
makeDict :: IO (Dictionary Text)
makeDict = do
  zips <- readUTF8line ".zipcode.out"
  return $ map toArray zips
  where toArray = (listArray (0,1) . split ',')
----------------------------------------------------------------------------------------------------
searchA :: Like.StringLike a => a -> Dictionary Text -> Answer (Dictionary Text)
searchA key dict = case (ordinary, verse) of
  ([_], [_]) -> Absolute ordinary
  _          -> let ret = uniq $ ordinary ++ verse in
                Probably (ret, length ret)
  where key'     = cutNumber key
        rev      = Tx.reverse key'
        ordinary = searchCore key' dict
        verse    = searchCore rev dict

cutNumber :: Like.StringLike a => a -> Text
cutNumber = toText . cut . toStr
  where toText key = cast key :: Text
        cut        = takeWhile (`notElem` "0123456789０１２３４５６７８９")
        toStr key  = cast key :: String

searchCore :: Text -> Dictionary Text -> Dictionary Text
searchCore key dict = (`St.execState` dict) $ searchST key'
  where key' = Like.castString key

telem :: Char -> Text -> Bool
telem c tx = isJust (Tx.findIndex (==c) tx)

searchST :: Text -> St.State (Dictionary Text) ()
searchST bs
  | bs == mempty = St.modify id
  | otherwise    = do
      let Just (ch, rest) = Tx.uncons bs
      dic <- St.get
      let filt = filter (\line -> ch `telem` (line!0)) dic
      case dic of
        [_] -> St.put dic
        _   -> do
          if null filt
            then St.put dic
            else St.put (searchCore rest filt)
----------------------------------------------------------------------------------------------------
addHit :: Char -> Hitting -> Hitting
addHit char hit' =
  Hitting { initial   = initial hit',
            pcode     = pcode hit',
            target    = newTarget,
            hit       = newHit,
            hitratio  = (newHit - (length newTarget)) % newHit,
            nohit     = nohit hit',
            continual = continual hit'
          }
  where newTarget = removeChar char (target hit')
        newHit    = (hit hit') + 1

guessHit :: Like.StringLike a => String -> Dictionary a -> (String, String)
guessHit f p = answer . head . reverse $ guessHitList f p
  where answer t = (initial t, pcode t)

guessHitList :: Like.StringLike a => String -> Dictionary a -> [Hitting]
guessHitList target dict = sort $ map (givePoint target) dict

givePoint :: Like.StringLike a => String -> DictLine a -> Hitting
givePoint baseStr gen = (`St.execState` initHit) $ do
  forM_ baseStr $ \char -> do
    hit <- St.get
    let newHit = if ([char] `isInfixOf` (target hit))
                 then addHit char hit
                 else hit { nohit = nohit hit + 1 }
    St.put newHit
  hit <- St.get
  St.put $ hit { continual = giveContinualPoint baseStr ad }
  where ad      = Like.castString (gen!0) :: String
        postal  = Like.castString (gen!1) :: String
        initHit = Hitting ad postal ad 0 (0%1) 0 0

-- guessHit :: String -> [[String]] -> 
-- todo: refine
giveContinualPoint :: String -> String -> Int
giveContinualPoint "" _ = 0
giveContinualPoint bsx@(b:bs) gen
  | bsx `isInfixOf` gen = length bsx
  | otherwise = giveContinualPoint bs gen

-- test "" _ = 0
-- test s g = (`St.execState` 0) $ do
  
  
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
    putStr   $ (cast ad) ++ ", "
    putStrLn $ toString $ guessHit (cast ad) <$> searchA ad dict

-- testIO :: IO ()
-- testIO = do
--   zips <- readUTF8File ".zipcode.out"
--   return $ map (listArray (0,1) . split ',') $ lines zips

-- testIO2 = do
--   zips <- B.pack <$> readUTF8File ".zipcode.out"
--   let dic = map (split ',') $ B.lines zips
--   return dic

-- testIO3 = do
--   dict <- testIO-- deserializeDict ".dict"
--   target <- readUTF8File ".test.address"
--   let result    = map (\n -> (n, Just $ length $ search' n dict)) $ lines target
--   hSetEncoding stdout utf8
--   mapM_ tuplePrint result
