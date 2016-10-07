module Kensin.Meibo (meiboOutput) where

import Util                             (makeMap, ketaNum, group)
import Util.ZenkakuHankaku              (toZenkaku)
import Control.Monad.Reader
import Data.Function                    (on)
import Data.List                        (sortBy)
import Kensin.Config
import Kensin.Base
import qualified Data.Map               as M

sortByHour :: [KensinData] -> [KensinData]
sortByHour    = sortBy (compare `on` toTime)

bunkaiMap :: [KensinData] -> M.Map Bunkai [KensinData]
bunkaiMap = makeMap bunkai id . filterSunday
  where filterSunday kd = let (sun, week) = splitSundayOrNot kd
                          in sun

sundayMeiboString :: Translator
sundayMeiboString kd = (`runReader` config) $ do
  comname <- meiboCommand <$> ask
  direct  <- meiboDirector <$> ask
  return $ latexCommand comname direct kd

meiboPageString :: Bunkai -> [KensinData] -> String
meiboPageString _ [] = ""
meiboPageString bunkai kds = (`runReader` config) $ do
  envname <- meiboEnvironment <$> ask
  return $ latexEnvironment envname
                            (Just $ bunkaiToStr bunkai ++ "分会")
                            $ concatMap sundayMeiboString kds

meiboOutput :: [KensinData] -> ReaderT Config IO ()
meiboOutput kds = do
  length' <- meiboLength <$> ask
  let bmap    = bunkaiMap kds
  let bunkais = [minBound..maxBound]
  forM_ bunkais $ \bunkai -> do
    case M.lookup bunkai bmap of
      Just persons -> do
        let list = group length' $ sortByHour persons
        forM_ list $ liftIO . putStrLn . meiboPageString bunkai
      Nothing      -> return ()
