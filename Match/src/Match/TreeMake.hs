{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Match.TreeMake (
  sortCRF
  , listToRT
  , Log
  , Logger
  , LoggerT
  , ErrorType (..)
  , RTK (..)
  , OyakataMap
  , KNumberMap) where

import           Control.Arrow                      ((>>>))
import           Control.Lens
import           Control.Monad                      (foldM, when)
import           Control.Monad.Trans                (lift)
import           Control.Monad.Trans.Writer.Strict  (Writer, WriterT
                                                    , tell)
import           Control.Monad.Trans.State          (StateT, put, get
                                                    , evalStateT)
import           Data.Foldable                      (toList)
import           Data.Monoid                        ((<>))
import qualified Data.Map.Strict                    as M
import           Data.Text                          (Text, unpack)
import qualified Data.Text                          as Tx
import           Match.Kumiai                       (Kumiai (..))

type OyakataMap  = M.Map Text (Text, Text)
type KNumberMap  = M.Map Text Kumiai
type Log         = [ErrorType]
type Logger a    = Writer Log a
type LoggerT m a = WriterT Log m a

data RelTree a = Node a (RelTree a) (RelTree a)
               | N deriving (Eq)

data ErrorType =
  OyakataHanUnMatch RTK RTK
  | OyakataNotFound RTK Text
  | ChildNotFound Text Int
  | RevOrder RTK Text
  | Something Int
  deriving (Show, Eq)

instance Show a => Show (RelTree a) where
  show N = "_"
  show (Node a l r) = "(" ++ str ++ ")"
    where
      lis = [show a, show l, show r]
      str = unwords lis

instance Functor RelTree where
  _ `fmap` N = N
  f `fmap` (Node a l r) = Node (f a) (f `fmap` l) (f `fmap` r)

instance Foldable RelTree where
  foldMap _ N = mempty
  foldMap f (Node a l r) = f a <> foldMap f l <> foldMap f r

instance Traversable RelTree where
  traverse _ N = pure N
  traverse f (Node a l r) =
    Node <$> f a <*> traverse f l <*> traverse f r

regularN :: Text -> Text
regularN = Tx.justifyRight 7 '0'

rknum :: Kumiai -> Text
rknum = regularN . (^. #number)

-- 新たに単独を追加する場合
-- testcase2の場合で、A・Eの後にFを追加する
-- A---E---F
-- |
-- B---C
-- |
-- D
append :: a -> RelTree a -> RelTree a
append a N = Node a N N
append new (Node a l r) = Node a l (append new r)
{-# INLINE append #-}

sameHanP :: RTK -> RTK -> Bool
sameHanP new oya = (bc new == bc oya) && (h new == h oya)
  where
    bc = (^. #bunkaiCode) . runK
    h  = (^. #han) . runK

hasTree :: Eq a => a -> RelTree a -> Bool
hasTree x rt = or ((==x) <$> rt)

addR :: RTK -> RelTree RTK -> RTK -> Logger (RelTree RTK)
addR _ N _ = return N
addR oya (Node a l r) new
  | (oya == a) && (sameHanP new oya) =
    return $ Node a (append new l) r
  | otherwise = do
      l' <- addR oya l new
      r' <- addR oya r new
      return $ Node a l' r'
{-# INLINE addR #-}

newtype RTK = RTK { runK :: Kumiai }

instance Eq RTK where
  rk1 == rk2 = (runK rk1) ^. #number == (runK rk2) ^. #number

instance Show RTK where
  show rtk = unpack $ "R(" <> (runK rtk) ^. #number <> ")"

hasPendingItem :: Text -> [(Text, RTK)] -> [RTK]
hasPendingItem tx = filter (fst >>> (== tx)) >>> map snd

insertRT :: OyakataMap -> KNumberMap
  -> RelTree RTK -> Kumiai
  -> StateT [(Text, RTK)] (Writer Log) (RelTree RTK)
insertRT om km rt x = do
  let rtkx  = RTK x
  let tell' = lift . tell
  let key   = rknum x
  -- 付き情報があるかどうか。
  let isRelational = snd <$> key `M.lookup` om
  -- stateには親方より先に現れた子方を入れておく。
  state <- get
  tree <- case isRelational  of
            -- 付きでない場合
            Nothing -> return $ append rtkx rt
            -- 付きの場合
            Just oyakataN ->
              case regularN oyakataN `M.lookup` km of
                -- 親方の番号から、親方の基幹情報 (oyakata) を取る。
                Just oyakata -> do
                  let rtko = RTK oyakata
                  case (hasTree rtko rt, sameHanP rtkx rtko) of
                    -- 親方と子方の分会または班が異なる場合。エラーと判断し、
                    -- OyakataHanUnMatchを発行したうえで、子方のノードを最後尾に
                    -- 配置する。
                    (_, False) -> do
                      tell' [OyakataHanUnMatch rtkx rtko]
                      return $ append rtkx rt
                    -- RelTreeの中にすでに親方が入っており、親方と子方が
                    -- 同じ分会・班である場合。問題がないので、親方につける
                    -- ように配置する。
                    (True, True)  -> lift $ addR rtko rt rtkx
                    -- 親方はまだRelTreeの中に入っていないが、分会・班が同じの
                    -- 場合。いったん、RelTreeには入れず
                    -- Stateに保管し、親方がRelTreeに入るタイミングで付け足していく。
                    (False, True) -> do
                      tell' [RevOrder rtkx (regularN oyakataN)]
                      put $ (oyakataN, rtkx) : state
                      return rt
                -- 親方の番号から、親方の基幹情報 (oyakata) を取れない場合、
                -- OyakataNotFoundを発行し、記録しておく。また、RelTree上には親方が
                -- 配置されていないので、RelTreeの最後尾にノードを配置し、その子方も
                -- 配置していく。
                Nothing -> do
                  tell' [OyakataNotFound rtkx oyakataN]
                  return $ append rtkx rt
  let pendings = hasPendingItem key state
  lift $ foldM (addR rtkx) tree pendings

listToRT :: OyakataMap -> KNumberMap -> [Kumiai]
 -> Logger (RelTree RTK)
listToRT rm km = (`evalStateT` []) . foldM (insertRT rm km) N

sortCRF :: OyakataMap -> KNumberMap -> [Kumiai] -> Logger [Kumiai]
sortCRF om km x = do
  rtk <- listToRT om km x
  return $ map runK (toList rtk)

-- A・B・Cがそれぞれ単独の場合
testcase1 = Node "A" N (Node "B" N (Node "C" N N))

-- B・C・XがA付、DがB付、Eが単独の場合
-- A---B---D
-- |   |
-- |   C---Y
-- |   |   |
-- |   |   Z
-- |   X
-- E
-- A | B | D
--   | C | Y
--   |   | Z
--   | X
-- E
-- ("A" ("B" ("D" _ _) ("C" _ _)) ("E" _ _))
testcase2 = Node "A" (Node "B" (Node "D" N N) (Node "C" (Node "Y" N (Node "Z" N N)) (Node "X" N N))) (Node "E" N N)
