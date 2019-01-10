{-# LANGUAGE GADTs                    #-}
module Util.MonadPath where

import           Control.Monad.Skeleton
import qualified Data.List                  as DL
import           Data.List.Split            (splitOn)
import           Data.Vector                ((!))
import qualified Data.Vector                as V

type FP = (V.Vector FilePath, Int)
type MonadFilePath = DirectoryM FP
type DirectoryM = Skeleton InnerDM

data InnerDM x where
  Up       :: FP -> InnerDM FP
  Down     :: FP -> InnerDM FP
  Init     :: FP -> InnerDM FP
  Bottom   :: FP -> InnerDM FP
  Length   :: FP -> InnerDM Int
  Repeat   :: Int -> (FP -> InnerDM FP) -> FP -> InnerDM FP
  PWD      :: FP -> InnerDM FilePath
  FullPath :: FP -> InnerDM FilePath

up           = bone . Up
down         = bone . Down
initial      = bone . Init
pwd          = bone . PWD
fullpath     = bone . FullPath
bottom       = bone . Bottom
rep i s      = bone . (Repeat i s)
pathLength   = bone . Length

runDirectoryM :: DirectoryM a -> a
runDirectoryM m =
  case debone m of
    Repeat 0 _ fp :>>= k ->
      runDirectoryM $ k fp
    Repeat n f fp :>>= k ->
      runDirectoryM $ (bone (f fp) >>= rep (n-1) f >>= k)
    Up (v, i) :>>= k ->
      runDirectoryM $ k $ (v, i - 1)
    Down (v, i) :>>= k   ->
      runDirectoryM $ k $ (v, i + 1)
    Init (v, _) :>>= k   ->
      runDirectoryM $ k $ (v, 0)
    PWD (v, i) :>>= k ->
      runDirectoryM $ k $ v ! (i - 1)
    Bottom (v, _) :>>= k ->
      runDirectoryM $ k $ (v, V.length v)
    FullPath (v, i) :>>= k -> do
      let xlist = map (v!) [0..i-1]
      runDirectoryM $ k $ DL.intercalate "/" xlist
    Length (v, _) :>>= k ->
      runDirectoryM $ k $ V.length v
    Return a -> a

monadicPath :: FilePath -> MonadFilePath
monadicPath fp = return $ (V.fromList $ splitOn "/" fp, 0)

-- hoge2 = "y:/ro/dee-Gov/62公文書(hoge)/030306_hoge/foo/喪失"
-- hoge2M = monadicPath hoge2

-- hoge = "y:/ro/dee-Gov/62公文書(hoge)/030306_hoge/foo"
-- hogeM = monadicPath hoge
