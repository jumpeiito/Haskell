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

concatnate :: Char -> [String] -> String
concatnate _ [] = ""
concatnate _ [x] = x
concatnate c (x:xs) = x ++ [c] ++ concatnate c xs

sundayMeiboString :: Translator
sundayMeiboString kd = (`runReader` config) $ do
  comname <- meiboCommand <$> ask
  return $ latexCommand comname [ Name, Furigana, Time, Nonpaylist, Paylist, Amount ] kd

meiboPageString :: Bunkai -> [KensinData] -> String
meiboPageString bunkai kds = (`runReader` config) $ do
  envname <- meiboEnvironment <$> ask
  return $ latexEnvironment envname
                            (Just bstr)
                            $ concatMap sundayMeiboString kds
  where bstr = bunkaiToStr bunkai ++ "分会"

meiboOutput :: [KensinData] -> ReaderT Config IO ()
meiboOutput kds = do
  length' <- meiboLength <$> ask
  let bmap    = bunkaiMap kds
  let bunkais = [minBound..maxBound]
  forM_ bunkais $ \bunkai -> do
    let Just persons = sortByHour <$> M.lookup bunkai bmap
    forM_ (group length' persons) (liftIO . putStrLn . meiboPageString bunkai)
