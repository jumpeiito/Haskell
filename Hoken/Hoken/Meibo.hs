{-# LANGUAGE OverloadedStrings #-}

module Hoken.Meibo  where

import Util                     hiding ((&&&))
import Hoken.Base               (Person (..), config, MeiboMap)
import Hoken.Parser             (splitAddress)
import Hoken.Secrets            ((<<|>>))
import qualified Hoken.Secrets  as Sec
import qualified Meibo.Base     as Meibo
import qualified Data.Map       as Map
import Control.Arrow            ((&&&))
import Data.List                (isPrefixOf, find)
import Data.Maybe               (fromMaybe)
import Util.Telephone           (telFuncPure, Telephone (..), telString, telMap)
import Control.Monad.State
import Text.Parsec              hiding (Line, State)
import Text.Parsec.String
import Test.Hspec

hasTel :: Telephone -> Meibo.Line -> Bool
hasTel telkey line = telkey `elem` Meibo.tel line
 
-- 名簿上のデータを引っ張ってくる。
-- 電話番号からの方が一義的に決まるので、電話番号からの検索を優先し、
-- それに引っ掛からない場合、名前からの検索を行う。
toMeiboData :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData p mp = toMeiboData3 p mp `mplus` toMeiboData2 p mp

-- 名前から名簿を絞りこむ。tmdのラッパー関数。
toMeiboData2 :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData2 p mp = case execState (tmd p) myMap of
  [x] -> Just x
  _   -> Nothing
  where Just myMap = Map.lookup (bunkai p) mp

-- 名前を1文字ごとに分解し、名簿をしぼりこんでいく。
tmd :: Person -> State [Meibo.Line] ()
tmd p = do
  let name' = name p
  forM_ name' $ \char -> do
    target <- get
    case filter ((char `elem`) . Meibo.name) target of
      -- ヒットしない場合、元に戻す。
      []  -> put target
      x   -> put x

-- 電話番号から名簿を絞りこむ。
toMeiboData3 :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData3 p mp =
  let Just targetList = Map.lookup (bunkai p) mp
  in case phone p of
    Nothing     -> Nothing
    Just telnum ->
      -- 名簿では京都の市外局番から始まる場合、075-を付けていないので、
      -- キーから075-を外す。
      let telnum' | "075-" `isPrefixOf` telString telnum = drop 4 `telMap` telnum
                  | otherwise = telnum
      in find (hasTel telnum') targetList

(<~~>) (p, func) (smp, func2) =
  func p <<|>> fromMaybe "" (func2 <$> Map.lookup (number p) smp)

toLatex :: Person -> Sec.SecretMap -> String
toLatex p smp = latexCom "Joseki" [name', sum', head']
  where name' = (p, name) <~~> (smp, Sec.name)
        sum'  = ketaNum $ show $ feeSum p
        head' = ketaNum $ show $ head $ feeList p

-- 7ケタの数字を郵便番号風に変える。
-- eg. 6000000 => 600-0000
regularPostal :: String -> String
regularPostal postal = pre ++ "-" ++ post
  where (pre, post) = (take 3 &&& drop 3) postal

(<~) :: (a -> [Char]) -> Maybe a -> [Char]
(<~) f mp = fromMaybe "" $ f <$> mp

toString :: Person -> MeiboMap -> Sec.SecretMap -> String
toString p mp smp = latexCom "personallabel" arguments
  where arguments = [ regularPostal pt, ad1, ad2, name p ]
        meiboData = toMeiboData p mp
        ad = (Meibo.ad <~ meiboData, id) <~~> (smp, Sec.ad1)
        pt = (Meibo.postal <~ meiboData, id) <~~> (smp, Sec.ad2)
        (ad1, ad2) = splitAddress ad

toDebug :: Person -> MeiboMap -> String
toDebug p mp = latexCom "debug" arguments
  where arguments = [ pt
                    , ad1
                    , ad2
                    , name p
                    , number p
                    , bunkai p
                    , feeStr p
                    , show $ feeSum p]
        meiboData = toMeiboData p mp
        ad = Meibo.ad <~ meiboData
        pt = Meibo.postal <~ meiboData
        (ad1, ad2) = splitAddress ad
