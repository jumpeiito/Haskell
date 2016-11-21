module Hoken.Meibo  where

import Util                     hiding ((&&&))
import Hoken.Base               (Person (..), config, MeiboMap)
import Hoken.Parser             (splitAddress)
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
 
toMeiboData :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData p mp = toMeiboData3 p mp `mplus` toMeiboData2 p mp

toMeiboData2 :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData2 p mp = case execState (tmd p) myMap of
  [x] -> Just x
  _   -> Nothing
  where Just myMap = Map.lookup (bunkai p) mp

tmd :: Person -> State [Meibo.Line] ()
tmd p = do
  let name' = name p
  forM_ name' $ \char -> do
    target <- get
    case filter ((char `elem`) . Meibo.name) target of
      []  -> put target
      x   -> put x

toMeiboData3 :: Person -> MeiboMap -> Maybe Meibo.Line
toMeiboData3 p mp =
  let Just targetList = Map.lookup (bunkai p) mp
  in case phone p of
    Nothing     -> Nothing
    Just telnum ->
      let telnum' | "075-" `isPrefixOf` telString telnum = drop 4 `telMap` telnum
                  | otherwise = telnum
      in find (hasTel telnum') targetList

toLatex :: Person -> String
toLatex p = "\\Joseki{" ++ name' ++ "}{" ++ sum' ++ "}{" ++ head' ++ "}"
  where name' = name p
        sum'  = ketaNum $ show $ feeSum p
        head' = ketaNum $ show $ head $ feeList p

regularPostal :: String -> String
regularPostal postal = pre ++ "-" ++ post
  where (pre, post) = (take 3 &&& drop 3) postal

toString :: Person -> MeiboMap -> String
toString p mp = latexCom "personallabel" arguments
  where arguments = [ regularPostal pt, ad1, ad2, name p ]
        meiboData = toMeiboData p mp
        ad = fromMaybe "" $ Meibo.ad <$> meiboData
        pt = fromMaybe "" $ Meibo.postal <$> meiboData
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
        ad = fromMaybe "" $ Meibo.ad <$> meiboData
        pt = fromMaybe "" $ Meibo.postal <$> meiboData
        (ad1, ad2) = splitAddress ad
