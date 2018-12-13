{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad                      (foldM, when)
import           Control.Monad.Trans                (lift)
import           Control.Monad.Trans.Writer.Strict  (Writer, WriterT
                                                    , tell)
import           Control.Monad.Trans.State          (StateT, put, get
                                                    , evalStateT)
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
  f `fmap` N = N
  f `fmap` (Node a l r) = Node (f a) (f `fmap` l) (f `fmap` r)

instance Foldable RelTree where
  foldMap _ N = mempty
  foldMap f (Node a l r) = f a <> foldMap f l <> foldMap f r

regularN :: Text -> Text
regularN = Tx.justifyRight 7 '0'

rknum :: Kumiai -> Text
rknum = regularN . kNumber

toList :: RelTree a -> [a]
toList = foldr (:) []

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
    bc = kBunkaiCode . runK
    h  = kHan . runK

hasTree :: Eq a => a -> RelTree a -> Bool
hasTree x rt = foldr (||) False ((==x) <$> rt)

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
  show rtk = unpack $ "R(" <> kNumber (runK rtk) <> ")"

hasPendingItem :: Text -> [(Text, RTK)] -> [RTK]
hasPendingItem _ [] = []
hasPendingItem tx (x:xs)
  | tx == fst x = snd x : hasPendingItem tx xs
  | otherwise   = hasPendingItem tx xs

pendingsClean :: RTK -> RelTree RTK -> [RTK] -> Logger (RelTree RTK)
pendingsClean oya = foldM (addR oya)

insertRT :: OyakataMap -> KNumberMap
  -> RelTree RTK -> Kumiai
  -> StateT [(Text, RTK)] (Writer Log) (RelTree RTK)
insertRT om km rt x = do
  let rtkx = RTK x
  let key  = rknum x
  -- 付き情報があるかどうか。
  let isRelational = snd <$> key `M.lookup` om
  -- stateには親方より先に現れた子方を入れておく。
  state <- get
  case (isRelational, hasPendingItem key state)  of
    -- 付きでない場合
    (Nothing, pendings) -> do
      -- RelTreeの最後尾に配置。
      let rt' = append rtkx rt
      -- このノードの子方で先に現れていた場合、このノードの左側につけていく。
      lift $ pendingsClean rtkx rt' pendings
    -- 付きの場合
    (Just oyakataN, pendings) ->
      case regularN oyakataN `M.lookup` km of
        -- 親方の番号から、親方の基幹情報 (oyakata) を取る。
        Just oyakata ->
          -- RelTreeの中にすでに親方が入っているか。
          if hasTree (RTK oyakata) rt
            -- 入っている場合は問題がないので、RelTreeに挿入し、さらにその
            -- 子方がいる場合も、RelTreeに挿入していく。
            then do rt' <- lift $ addR (RTK oyakata) rt rtkx
                    lift $ pendingsClean rtkx rt' pendings
            -- 親方が入っていない場合は、RelTreeに挿入せず、stateに保管
            -- していき、元のRelTreeを返す。
            else do lift $ tell [RevOrder rtkx (regularN oyakataN)]
                    put $ (oyakataN, rtkx) : state
                    return rt
        -- 親方の番号から、親方の基幹情報 (oyakata) を取れない場合、
        -- OyakataNotFoundを発行し、記録しておく。また、RelTree上には親方が
        -- 配置されていないので、RelTreeの最後尾にノードを配置し、その子方も
        -- 配置していく。
        Nothing -> do
          lift $ tell [OyakataNotFound rtkx oyakataN]
          let rt' = append rtkx rt
          lift $ pendingsClean rtkx rt' pendings

listToRT :: OyakataMap -> KNumberMap -> [Kumiai]
 -> Logger (RelTree RTK)
listToRT rm km = (`evalStateT` []) . foldM (insertRT rm km) N

sortCRF :: OyakataMap -> KNumberMap -> [Kumiai] -> Logger [Kumiai]
sortCRF om km x = do
  rtk <- listToRT om km x
  return $ map runK (toList rtk)

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

