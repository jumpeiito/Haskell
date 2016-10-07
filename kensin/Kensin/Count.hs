module Kensin.Count ( jusinShowLine
                    , translateJusin) where

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

makePayListCombinator :: ( [KensinOption] -> CfgReader [Bool] ) ->
                         [KensinOption] ->
                         Reader Config [Int]
makePayListCombinator f1 kop = do
  ls <- f1 kop
  let boolList = indexAdd ls
  return $ map fst $ filter snd boolList

pBoolList, npBoolList :: [KensinOption] -> CfgReader [Bool]
pBoolList kop = do
  opts <- map fst . payList <$> ask
  return $ map (`elem` kop) opts

npBoolList kop = do
  opts <- map snd . nonPayList <$> ask
  return $ map (any (`elem` kop)) opts

makeNonPayList, makePayList :: [KensinOption] -> Reader Config [Int]
makeNonPayList = makePayListCombinator npBoolList
makePayList    = makePayListCombinator pBoolList

keyContains :: (KensinData -> KParse Option) -> Option -> KensinBool
keyContains f opList kd = any bool opList
  where bool n = case elem n <$> f kd of
                   Right x -> x
                   _ -> False

nonPayContains :: Option -> KensinBool
nonPayContains [] = const False
nonPayContains op = keyContains nonPay op

payContains :: Option -> KensinBool
payContains = keyContains pay

kensinBoolBuilder :: (Config -> [KensinOption]) -> KensinBool
kensinBoolBuilder f kd = (`runReader` config) $ do
  sym <- f <$> ask
  nop <- makeNonPayList sym
  op  <- makePayList sym
  -- nopが[]の場合、||の左辺は必ずFalseを返す。
  return $ nonPayContains nop kd || payContains op kd

ladies1P, ladies2P, jinpaiP, cameraP, tokP :: KensinBool
ladies1P = kensinBoolBuilder optionLadies1
ladies2P = kensinBoolBuilder optionLadies2
jinpaiP  = kensinBoolBuilder optionJinpai
cameraP  = kensinBoolBuilder optionCamera
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
