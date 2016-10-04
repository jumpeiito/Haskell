module Kensin.Count (jusinShowLine, translateJusin) where

import Util                             (makeMap)
import Kensin.Base
import Kensin.Config
import Data.Array
import Data.List
import Control.Monad.Reader
import qualified Data.Map               as M
import qualified Text.Printf            as TP

indexAdd :: [a] -> [(Int, a)]
indexAdd ls = assocs $ listArray (1, length ls) ls

makePayListCombinator :: ( [KensinOption] -> Config -> [Bool] ) ->
                         [KensinOption] ->
                         Reader Config [Int]
makePayListCombinator f1 kop = do
  ls <- f1 kop <$> ask
  let boolList = indexAdd ls
  return $ map fst $ filter snd boolList

nonPayBoolList :: [KensinOption] -> Config -> [Bool]
nonPayBoolList kop cfg = 
  let opts = map snd $ nonPayList cfg
  in map (any (`elem` kop)) opts

payBoolList :: [KensinOption] -> Config -> [Bool]
payBoolList kop cfg =
  let opts = map fst $ payList cfg
  in map (`elem` kop) opts

makeNonPayList, makePayList :: [KensinOption] -> Reader Config [Int]
makeNonPayList = makePayListCombinator nonPayBoolList
makePayList    = makePayListCombinator payBoolList

keyContains :: (KensinData -> KParse Option) -> Option -> KensinBool
keyContains f opList kd = any bool opList
  where bool n = case elem n <$> f kd of
                   Right x -> x
                   _ -> False

nonPayContains :: Option -> KensinBool
nonPayContains = keyContains nonPay

payContains :: Option -> KensinBool
payContains = keyContains pay

ladiesP :: Option -> Option -> KensinBool
ladiesP npList pList kd = nonPayContains npList kd || payContains pList kd

ladies1P, ladies2P, jinpaiP, cameraP, tokP :: KensinBool
ladies1P = ladiesP ["5", "6", "7", "8", "9"] ["8", "9", "10"] -- 乳がん・子宮がん
ladies2P = ladiesP ["5", "6", "8", "9"] ["8", "9"]            -- 乳がんのみ
jinpaiP  = payContains ["13", "14"]
cameraP  = payContains ["11"]
tokP kd  = old kd>=40 && old kd<75

countIf :: (a -> Bool) -> [a] -> Int
countIf f = length . filter f

numberCount :: [KensinData] -> [(String, Int)]
numberCount kds =
  map (\(str, f) -> (str, countIf f kds))
      [ ("全女性検診", ladies1P)
      , ("乳がんのみ", ladies2P)
      , ("アスベスト", jinpaiP)
      , ("胃カメラ", cameraP)]

makeKensinMap :: [KensinData] -> M.Map (Maybe String) [KensinData]
makeKensinMap = makeMap sortKey id

translateJusin :: [KensinData] -> [(Maybe String, [(String, Int)])]
translateJusin =
  map count' . M.toList . makeKensinMap
  where count' (k, v) = (k, ("全受診者", length v):numberCount v)

jusinShowPair :: (String, Int) -> String
jusinShowPair (title, len)
  | len == 0  = "------------"
  | otherwise = TP.printf "%s: %d" title len

jusinShowLine :: (Maybe String, [(String, Int)]) -> String
jusinShowLine (Nothing, _) = ""
jusinShowLine (Just date, pairs) =
  TP.printf "%s :: %s\t:: %s" date' ps date'
  where ps = intercalate "\t" $ map jusinShowPair pairs
        date' = drop 5 date
