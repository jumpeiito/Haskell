{-# LANGUAGE OverloadedStrings #-}

module Hoken.Meibo  where

import Util                     hiding ((&&&))
import Hoken.Base               (Person (..), config, MeiboMap)
import Hoken.Parser             (splitAddress, pobjectParse)
import Hoken.Secrets            ((<<|>>), Secrets (..))
-- import Meibo.Base               (Line (..))
import qualified Meibo.Base     as MB
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

(<~~) :: (a -> b) -> (Person, Map.Map String a) -> Maybe b
(<~~) f (p, smp) = f <$> Map.lookup (number p) smp

toLatex :: Person -> Sec.SecretMap -> String
toLatex p smp = latexCom "Joseki" [name', sum', head']
  where name' = Sec.name <~~ (p, smp) <<|>> Just (name p)
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
  where arguments = [ regularPostal pt, ad1, ad2, name' ]
        meiboData = toMeiboData p mp
        ad           = fromMaybe "" $ Meibo.ad <$> meiboData
        (ad1', ad2') = splitAddress ad
        name'        = Sec.name <~~ (p, smp) <<|>> Just (name p)
        pt           = Sec.post <~~ (p, smp) <<|>> (Meibo.postal <$> meiboData)
        ad1          = Sec.ad1  <~~ (p, smp) <<|>> Just ad1'
        ad2          = Sec.ad2  <~~ (p, smp) <<|>> Just ad2'

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

testcase1 = "6醍50101伊東090-1901-0111＊4120041200 82400"
testcase2 = "6醍50102伊東090-1901-0111＊4120041200 82400"
testp1 = either (const PersonError) id $ parse pobjectParse "" testcase1
testp2 = either (const PersonError) id $ parse pobjectParse "" testcase2

testgen = map Sec.toSecretPerson [["50101", "Carlo", "5010000", "KyotoCity", "MinamiWard"]]
testsmp = makeSingleMap Sec.number id testgen

testLine = MB.Line { MB.bunkai = "点在"
                   , MB.bknum  = "50"
                   , MB.han    = ""
                   , MB.kind   = ""
                   , MB.hancho = Just ""
                   , MB.gen    = ""
                   , MB.name   = "7777"
                   , MB.nameP  = ("", "")
                   , MB.ad     = "カリフォルニア州ロサンゼルス3-2プリプリハウス302号"
                   , MB.tel    = [Mobile "090-1901-0111"]
                   , MB.work   = ""
                   , MB.exp    = ""
                   , MB.furi   = ""
                   , MB.birthS = ""
                   , MB.birth  = Nothing
                   , MB.year   = Nothing
                   , MB.postal = "6000000"}
testmp = makeMap MB.bunkai id [testLine]           

toLatexSpec :: Spec
toLatexSpec = do
  describe "toLatex" $ do
    it "test1" $ testp1 `toLatex` testsmp `shouldBe` "\\Joseki{****}{82,400}{41,200}"
    it "test2" $ testp2 `toLatex` testsmp `shouldBe` "\\Joseki{伊東}{82,400}{41,200}"

toStringSpec :: Spec
toStringSpec = do
  describe "toString" $ do
    -- post -> ad1 -> ad2 -> name
    it "test1" $ toString testp1 testmp testsmp `shouldBe` "\\personallabel{501-0000}{KyotoCity}{MinamiWard}{Carlo}"
    it "test2" $ toString testp2 testmp testsmp `shouldBe` "\\personallabel{600-0000}{----}{@@@@}{****}"
