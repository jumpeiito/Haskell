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

sortByDay, sortByBunkai, sortByHour :: [KensinData] -> [KensinData]
sortByDay     = sortBy (compare `on` day)
sortByBunkai  = sortBy (compare `on` bunkai)
sortByHour    = sortBy (compare `on` toTime)

makeReceiptData :: (([KensinData], [KensinData]) -> [KensinData]) -> -- fst or snd
                   ([KensinData] -> [KensinData]) ->                 -- sort Function
                   [KensinData] ->
                   [KensinData]
makeReceiptData f sortf = sortf . filter hasAmount . f . splitSundayOrNot

receiptSunday, receiptWeekday :: [KensinData] -> [KensinData]
receiptSunday  = makeReceiptData fst sortByBunkai
receiptWeekday = makeReceiptData snd sortByDay

kensinDataToReceipt :: Translator
kensinDataToReceipt kd = (`runReader` config) $ do
  comname <- receiptCommand <$> ask
  return $ latexCommand comname [ dayStrWithSep '/' $ day kd
                                , bunkaiToStr $ bunkai kd
                                , name kd
                                , ketaNum $ show $ either (const 0) id $ amount kd ]
  
toReceiptPage :: [KensinData] -> String
toReceiptPage kds = (`runReader` config) $ do
  envname <- receiptEnvironment <$> ask
  return $ latexEnvironment envname
                            Nothing
                            $ concatMap kensinDataToReceipt kds

toReceipt :: [KensinData] -> String
toReceipt kds = concatMap toReceiptPage $ group 5 kds
