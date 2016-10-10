module Kensin.Meibo (meiboOutput, bunkaiMap) where

import Util                             (makeMap, ketaNum, group)
import Util.ZenkakuHankaku              (toZenkaku)
import Control.Monad.Reader
import Data.Function                    (on)
import Data.List                        (sortBy)
import Data.Maybe                       (mapMaybe)
import Kensin.Config
import Kensin.Base
import qualified Data.Map               as M

type BunkaiMap = M.Map Bunkai [KensinData]

sortByHour :: [KensinData] -> [KensinData]
sortByHour    = sortBy (compare `on` toTime)

bunkaiMap :: [KensinData] -> CfgReader BunkaiMap
bunkaiMap kd = do
  (sun, _) <- splitSundayOrNot kd
  return $ makeMap bunkai id sun

sundayMeiboString :: KensinData -> CfgReader String
sundayMeiboString kd = do
  comname <- meiboCommand <$> ask
  direct  <- meiboDirector <$> ask
  return $ latexCommand comname direct kd

meiboPageString :: Bunkai -> [KensinData] -> CfgReader String
meiboPageString _ [] = return ""
meiboPageString bunkai kds = do
  envname   <- meiboEnvironment <$> ask
  sundayStr <- mapM sundayMeiboString kds
  return $ latexEnvironment envname
                            (Just $ bunkaiToStr bunkai ++ "分会")
                            $ concat sundayStr

meiboBunkaiString :: Int -> BunkaiMap -> Bunkai -> CfgReader String
meiboBunkaiString len bmap bk = case M.lookup bk bmap of
  Just kd -> do
    let classed = group len $ sortByHour kd
    concat <$> mapM (meiboPageString bk) classed
  Nothing -> return ""

meiboOutput :: [KensinData] -> CfgReader String
meiboOutput kds = do
  length' <- meiboLength <$> ask
  bmap    <- bunkaiMap kds
  concat <$> mapM (meiboBunkaiString length' bmap) [minBound..maxBound]
