{-# LANGUAGE OverloadedStrings #-}
module Match.TreeMake (
  sortCRF
  , listToRT3
  , toList
  , Log
  , Logger
  , LoggerT
  , ErrorType (..)
  , RTK (..)
  , OyakataMap
  , KNumberMap) where

import           Control.Monad    (foldM, when)
import           Control.Monad.Trans.Writer.Strict
import           Data.Monoid      ((<>))
import qualified Data.Map.Strict  as M
import           Data.Text        (Text, unpack)
import qualified Data.Text                 as Tx
import           Match.Kumiai

type OyakataMap  = M.Map Text (Text, Text)
type KNumberMap  = M.Map Text Kumiai
type Log         = [ErrorType]
type Logger a    = Writer Log a
type LoggerT m a = WriterT Log m a

data RelTree a = Node a (RelTree a) (RelTree a)
               | N deriving (Eq)

data ErrorType =
  OyakataHanUnMatch RTK RTK
  | OyakataNotFound RTK
  | ChildNotFound Text
  | NeedToRepair RTK Text
  | DeleteNodeWithChildren RTK
  | Something Int
  deriving (Show, Eq)

instance Show a => Show (RelTree a) where
  show N = "_"
  show (Node a l r) = "(" ++ str ++ ")"
    where
      lis = [show a, show l, show r]
      str = unwords lis

instance Foldable RelTree where
  foldMap _ N = mempty
  foldMap f (Node a l r) = f a <> foldMap f l <> foldMap f r

-- A・B・Cがそれぞれ単独の場合
testcase1 = Node "A" N (Node "B" N (Node "C" N N))

-- B・CがA付、DがB付、Eが単独の場合
-- A---E
-- |
-- B---C
-- |
-- D
-- ("A" ("B" ("D" _ _) ("C" _ _)) ("E" _ _))
testcase2 = Node "A" (Node "B" (Node "D" N N) (Node "C" N N)) (Node "E" N N)

regularN :: Text -> Text
regularN = Tx.justifyRight 7 '0'

singleton :: a -> RelTree a
singleton a = Node a N N

toList :: RelTree a -> [a]
toList N = []
toList (Node a N N) = [a]
toList (Node a l r) = [a] ++ toList l ++ toList r

-- 新たに単独を追加する場合
-- testcase2の場合で、A・Eの後にFを追加する
-- A---E---F
-- |
-- B---C
-- |
-- D
append :: a -> RelTree a -> RelTree a
append a N = singleton a
append new (Node a l r) = Node a l (append new r)
{-# INLINE append #-}

-- B・CがA付、DがB付、Eが単独の場合で,新たにXをBCより先に追加する。
-- A---E
-- |
-- B---C
-- |
-- D
-- というRelTreeを下記のように変更する。
-- A---E
-- |
-- X---B---C
--     |
--     D

sameHanP :: RTK -> RTK -> Bool
sameHanP new oya = (bc new == bc oya) && (h new == h oya)
  where
    bc = kBunkaiCode . runK
    h  = kHan . runK

hasTree :: Eq a => a -> RelTree a -> Bool
hasTree _ N = False
hasTree x (Node a l r) = x == a || hasTree x l || hasTree x r

addR :: RTK -> RelTree RTK -> RTK -> Logger (RelTree RTK)
addR _ N _ = return N
addR oya (Node a l r) new
  | (oya == a) && (sameHanP new oya) =
    return $ Node a (append new l) r
  | otherwise = do
      l' <- addR oya l new
      r' <- addR oya r new
      when (oya == a) $ tell [OyakataHanUnMatch new oya]
      return $ Node a l' r'
{-# INLINE addR #-}

newtype RTK = RTK { runK :: Kumiai }

instance Eq RTK where
  rk1 == rk2 = kNumber (runK rk1) == kNumber (runK rk2)

instance Show RTK where
  show rtk = unpack $ "R(" <> (kNumber $ runK rtk) <> ")"

hasPendingItem :: Text -> [(Text, RTK)] -> [RTK]
hasPendingItem _ [] = []
hasPendingItem tx (x:xs)
  | tx == fst x = snd x : hasPendingItem tx xs
  | otherwise   = hasPendingItem tx xs

insertRT :: OyakataMap -> KNumberMap
  -> (RelTree RTK, [(Text, RTK)]) -> Kumiai
  -> Logger (RelTree RTK, [(Text, RTK)])
insertRT om km (rt, state) x = do
  let rtkx = RTK x
  let key  = rknum x
  let isRelational = snd <$> key `M.lookup` om
  case (isRelational, hasPendingItem key state)  of
    (Nothing, []) ->
      return (append rtkx rt, state)
    (Nothing, pendings) -> do
      let rt' = append rtkx rt
      newRT <- foldM (addR rtkx) rt' pendings
      return (newRT, state)
    (Just oyakataN, pendings) -> do
      case regularN oyakataN `M.lookup` km of
        Just oyakata -> do
          if hasTree (RTK oyakata) rt
            then do rt'   <- addR (RTK oyakata) rt rtkx
                    newRT <- foldM (addR rtkx) rt' pendings
                    return (newRT, state)
            else do tell [NeedToRepair rtkx (regularN oyakataN)]
                    return (rt, (oyakataN, rtkx) : state)
        Nothing -> do
          tell [OyakataNotFound rtkx]
          let rt' = append rtkx rt
          newRT <- foldM (addR rtkx) rt' pendings
          return (newRT, state)

listToRT3 :: OyakataMap -> KNumberMap -> [Kumiai] -> Logger (RelTree RTK)
listToRT3 rm km ks = do
  let ((rtk, _), log) = runWriter $ foldM (insertRT rm km) (N, []) ks
  tell log
  return rtk

sortCRF :: OyakataMap -> KNumberMap -> [Kumiai] -> Logger [Kumiai]
sortCRF om km x = do
  rtk <- listToRT3 om km x
  return $ map runK (toList rtk)

rknum :: Kumiai -> Text
rknum = regularN . kNumber
