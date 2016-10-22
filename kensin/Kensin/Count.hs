module Kensin.Count ( jusinShowLine
                    , translateJusin
                    , ladies1P
                    , cameraP
                    , jinpaiP) where

import Util                             (makeMap)
import Kensin.Base
import Kensin.Config
import Data.Array
import Data.List
import Control.Monad.Reader
import qualified Data.Map               as M
import qualified Text.Printf            as TP

indexAdd :: [a] -> [(Int, a)]
indexAdd ls = assocs $ listArray (0, length ls - 1) ls

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

kensinBoolBuilder :: (Config -> [KensinOption]) -> KensinData -> CfgReader Bool
kensinBoolBuilder f kd = do
  sym <- f <$> ask
  nop <- makeNonPayList sym
  op  <- makePayList sym
  -- nopが[]の場合、||の左辺は必ずFalseを返す。
  return $ nonPayContains nop kd || payContains op kd

ladies1P, ladies2P, jinpaiP, cameraP :: KensinData -> CfgReader Bool
allP _   = return True
ladies1P = kensinBoolBuilder optionLadies1
ladies2P = kensinBoolBuilder optionLadies2
jinpaiP  = kensinBoolBuilder optionJinpai
cameraP  = kensinBoolBuilder optionCamera

tokP :: KensinBool
tokP kd  = old kd>=40 && old kd<75

countIf :: (a -> CfgReader Bool) -> [a] -> CfgReader Int
countIf f kds = length <$> filterM f kds

numberCountRow :: [KensinData] -> CfgReader [Int]
numberCountRow kds = do
  mapM (`countIf` kds) [allP, ladies1P, ladies2P, jinpaiP, cameraP]
  
numberCount :: [KensinData] -> CfgReader [(String, Int)]
numberCount kds = do
  let strL  = ["全", "全女", "乳", "ア", "胃"]
  zip strL <$> numberCountRow kds

makeKensinMap :: [KensinData] -> M.Map (Maybe String) [KensinData]
makeKensinMap = makeMap sortKey id

translateJusin :: [KensinData] -> CfgReader [(Maybe String, [(String, Int)])]
translateJusin kd = do
  let alist = M.toList $ makeKensinMap kd
  let days  = map fst alist
  kds <- mapM numberCount $ map snd alist
  return $ zip days kds

jusinShowPair :: (String, Int) -> String
jusinShowPair (title, len)
  | len == 0  = "----"
  | otherwise = TP.printf "%s: %d" title len

jusinShowLine :: (Maybe String, [(String, Int)]) -> String
jusinShowLine (Nothing, _) = ""
jusinShowLine (Just date, pairs) =
  TP.printf "%s :: %s\t:: %s" date' ps date'
  where ps = intercalate "\t" $ map jusinShowPair pairs
        date' = drop 5 date
----------------------------------------------------------------------------------------------------
numberCountArray :: [KensinData] -> CfgReader (Array Int Int)
numberCountArray kds = listArray (0, 4) <$> numberCountRow kds

translateJusinArray :: [KensinData] -> CfgReader [(Maybe String, Array Int Int)]
translateJusinArray kds = do
  let alist = M.toList $ makeKensinMap kds
  let days  = map fst alist
  kd <- mapM numberCountArray $ map snd alist
  return $ zip days kd
  
showFigure :: (Int, Int) -> String
showFigure (black, white) =
  let bldiv = black `div` 10
      blmod = black `mod` 10
      whtop = 10 - blmod
      whdiv = (white - whtop) `div` 10
      whbot = white - whdiv * 10 - whtop
      fbld  = mconcat $ take bldiv $ repeat "●●●●●●●●●●\n"
      fblm  = mconcat $ take blmod $ repeat "●"
      fwht  = mconcat $ take whtop $ repeat "○"
      fwhd  = mconcat $ take whdiv $ repeat "○○○○○○○○○○\n"
      fwhb  = mconcat $ take whbot $ repeat "○"
  in fbld ++ fblm ++ fwht ++ "\n" ++ fwhd ++ fwhb
