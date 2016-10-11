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

type SortFunction  = [KensinData] -> [KensinData]
type FstOrSnd      = (([KensinData], [KensinData]) -> [KensinData]) 
type Filtering     = [KensinData] -> CfgReader [KensinData]

sortByDay, sortByBunkai, sortByHour :: SortFunction
sortByDay     = sortBy (compare `on` day)
sortByBunkai  = sortBy (compare `on` bunkai)
sortByHour    = sortBy (compare `on` toTime)

makeReceiptData :: FstOrSnd -> SortFunction -> Filtering
makeReceiptData f sortf kds = do
  pairs <- splitSundayOrNot kds
  return $ sortf $ filter hasAmount $ f pairs

receiptSunday, receiptWeekday :: Filtering
receiptSunday  = makeReceiptData fst sortByBunkai
receiptWeekday = makeReceiptData snd sortByDay

kensinDataToReceipt :: KensinData -> CfgReader String
kensinDataToReceipt kd = do
  comname <- receiptCommand <$> ask
  direct  <- receiptDirector <$> ask
  return $ latexCommand comname direct kd

toReceiptPage :: CfgTranslator
toReceiptPage kds = do
  envname <- receiptEnvironment <$> ask
  args    <- concatMapM kensinDataToReceipt kds
  return $ latexEnvironment envname Nothing args

toReceipt :: CfgTranslator
toReceipt kds = do
  len <- receiptLength <$> ask
  concatMapM toReceiptPage $ group len kds
