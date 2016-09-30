module Kensin.Receipt ( receiptSunday
                      , receiptWeekday
                      , toReceipt ) where

import Util                             (ketaNum, group)
import Util.Strdt                       (dayStrWithSep)
import Kensin.Base
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
kensinDataToReceipt kd = 
  latexCommand "writer" [ dayStrWithSep '/' $ day kd
                        , bunkaiToStr $ bunkai kd
                        , name kd
                        , ketaNum $ show $ either (const 0) id $ amount kd ]

toReceiptPage :: [KensinData] -> String
toReceiptPage kds = "\\begin{receiptPage}" ++
                    concatMap kensinDataToReceipt kds ++
                    "\\end{receiptPage}"

toReceipt :: [KensinData] -> String
toReceipt kds = concatMap toReceiptPage $ group 5 kds
