module Kensin.Check where

import Kensin.Config
import Kensin.Base
import Data.Array

guessYearClass :: KensinData -> YearClass
guessYearClass kd
  | old kd >= 40 = Over $ gender kd
  | otherwise    = Under $ gender kd

nonPayCheck :: Array Int [YearClass] -> KensinData -> Bool
nonPayCheck cfgList kd =
  let Right [kdnp] = nonPay kd 
  in guessYearClass kd `elem` cfgList ! kdnp
  
