module Kensin.Receipt ( receiptSunday
                      , receiptWeekday
                      , toReceipt ) where

import Util                             (ketaNum, group)
import Util.Strdt                       (dayStrWithSep)
import Control.Monad.Reader
import Kensin.Base
import Kensin.Config
import Data.Function                    (on)
import Data.List                        (sortBy)

type SortFunction = [KensinData] -> [KensinData]
type FstOrSnd     = (([KensinData], [KensinData]) -> [KensinData]) 

sortByDay, sortByBunkai, sortByHour :: SortFunction
sortByDay     = sortBy (compare `on` day)
sortByBunkai  = sortBy (compare `on` bunkai)
sortByHour    = sortBy (compare `on` toTime)

makeReceiptData :: FstOrSnd -> SortFunction -> [KensinData] -> [KensinData]
makeReceiptData f sortf = sortf . filter hasAmount . f . splitSundayOrNot

receiptSunday, receiptWeekday :: [KensinData] -> [KensinData]
receiptSunday  = makeReceiptData fst sortByBunkai
receiptWeekday = makeReceiptData snd sortByDay

kensinDataToReceipt :: Translator
kensinDataToReceipt kd = (`runReader` config) $ do
  comname <- receiptCommand <$> ask
  direct  <- receiptDirector <$> ask
  return $ latexCommand comname direct kd

toReceiptPage :: [KensinData] -> String
toReceiptPage kds = (`runReader` config) $ do
  envname <- receiptEnvironment <$> ask
  return $ latexEnvironment envname
                            Nothing
                            $ concatMap kensinDataToReceipt kds

toReceipt :: [KensinData] -> String
toReceipt kds = (`runReader` config) $ do
  len <- receiptLength <$> ask
  return $ concatMap toReceiptPage $ group len kds
