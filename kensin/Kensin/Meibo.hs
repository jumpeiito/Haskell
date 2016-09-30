module Kensin.Meibo (meiboOutput) where

import Util                             (makeMap, ketaNum, group)
import Util.ZenkakuHankaku              (toZenkaku)
import Control.Monad
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
sundayMeiboString kd = 
  latexCommand "meibo" [ name kd
                       , toZenkaku $ furigana kd
                       , toTime kd
                       , joinPay $ nonPay kd
                       , joinPay $ pay kd
                       , ketaNum $ either (const "") show $ amount kd ]
  where joinPay = either (const "") (concatnate '・')

meiboPageOutput :: Bunkai -> [KensinData] -> IO ()
meiboPageOutput bunkai kds = do
  putStrLn $ "\\begin{SundayMeibo}{" ++ bunkaiToStr bunkai ++ "分会}"
  forM_ kds (putStrLn . sundayMeiboString)
  putStrLn "\\end{SundayMeibo}"

meiboOutput :: [KensinData] -> IO ()
meiboOutput kds = do
  let bmap    = bunkaiMap kds
  let bunkais = [minBound..maxBound]
  forM_ bunkais $ \bunkai -> do
    let Just persons = sortByHour <$> M.lookup bunkai bmap
    forM_ (group 35 persons) (meiboPageOutput bunkai)
