{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}
module Util.MonadPath where

import           Control.Monad.State
import qualified Data.List                  as DL
import           Data.List.Split            (splitOn)
import           Data.Vector                ((!))
import qualified Data.Vector                as V

-- hoge2 = "y:/ro/dee-Gov/62公文書(hoge)/030306_hoge/foo/喪失"
-- hoge2F = initM hoge2

-- hoge = "y:/ro/dee-Gov/62公文書(hoge)/030306_hoge/foo"
-- hogeF = initM hoge

type InnerPath = ((V.Vector String), Int)
type MonadicPath a = State InnerPath a

initM :: FilePath -> InnerPath
initM fp = (V.fromList $ splitOn "/" fp, 0)

upM, downM, initialM, bottomM :: MonadicPath ()
upM = do
  (v, i) <- get
  if i == 0
    then put (v, i)
    else put (v, i - 1)
downM = do
  (v, i) <- get
  if i + 1 >= V.length v - 1
    then put (v, V.length v - 1)
    else put (v, i + 1)
initialM = do
  (v, _) <- get
  put (v, 0)
bottomM  = do
  (v, _) <- get
  put (v, V.length v - 1)

basenameM :: MonadicPath String
basenameM = do
  (v, i) <- get
  return $ v ! i

fullPathM :: MonadicPath String
fullPathM = do
  (v, i) <- get
  let xlist = map (v!) [0..i-1]
  return $ DL.intercalate "/" xlist

pathLengthM :: MonadicPath Int
pathLengthM = V.length <$> fst <$> get

addM :: [String] -> MonadicPath ()
addM dirs = do
  (v, i) <- get
  put (v `mappend` V.fromList dirs, i)

repM :: Int -> MonadicPath () -> MonadicPath ()
repM i f = sequence_ [f | _ <- [0..i-1]]

(../), (.../), (..../), (...../)   :: MonadicPath ()
(....../), (......./), (......../) :: MonadicPath ()
(../)       = upM
(.../)      = repM 2 upM
(..../)     = repM 3 upM
(...../)    = repM 4 upM
(....../)   = repM 5 upM
(......./)  = repM 6 upM
(......../) = repM 7 upM

(.//), (.///), (.////), (./////)   :: MonadicPath ()
(.//////), (.///////), (.////////) :: MonadicPath ()
(.//)       = downM
(.///)      = repM 2 downM
(.////)     = repM 3 downM
(./////)    = repM 4 downM
(.//////)   = repM 5 downM
(.///////)  = repM 6 downM
(.////////) = repM 7 downM

runFileM :: FilePath -> MonadicPath a -> a
runFileM fp m = m `evalState` initM fp

isLengthp :: Int -> MonadicPath Bool
isLengthp i = (==i) <$> (V.length . fst) <$> get
