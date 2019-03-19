module Util.ZenkakuHankaku where

import Control.Applicative
import Data.Maybe               (fromMaybe)
import Test.Hspec
import qualified Data.Map as M
import qualified System.IO                 as I
----------------------------------------------------------------------------------------------------
withReverse :: (String -> String) -> String -> String
withReverse f = reverse . f . reverse

----------------------------------------------------------------------------------------------------
preAlnum, postAlnum, preKigou, postKigou, preKana, postKana, preStr, postStr
  :: String
preAlnum  = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
postAlnum = ['ａ'..'ｚ'] ++ ['Ａ'..'Ｚ'] ++ ['０'..'９']
preKigou  = "$<>+@() ･+?:;=!#$%&/_^.､~~-ｰ"
postKigou = "＄＜＞＋＠（）　・＋？：；＝！＃＄％＆／＿＾．、〜～ーー"
preKana   = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｧｨｩｪｫｯｬｭｮ"
postKana  = "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲンァィゥェォッャュョ"
-- preKana2  = "ｶﾞｷﾞｸﾞｹﾞｺﾞｻﾞｼﾞｽﾞｾﾞｿﾞﾀﾞﾁﾞﾂﾞﾃﾞﾄﾞﾊﾞﾋﾞﾌﾞﾍﾞﾎﾞﾊﾟﾋﾟﾌﾟﾍﾟﾎﾟｳﾞ"
-- postKana2 = "ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポヴ"

kanaMap, kanaMap2 :: M.Map Char [Char]
kanaMap = M.fromList [ ('ｶ', "ガ")
                     , ('ｷ', "ギ")
                     , ('ｸ', "グ")
                     , ('ｹ', "ゲ")
                     , ('ｺ', "ゴ")
                     , ('ｻ', "ザ")
                     , ('ｼ', "ジ")
                     , ('ｽ', "ズ")
                     , ('ｾ', "ゼ")
                     , ('ｿ', "ゾ")
                     , ('ﾀ', "ダ")
                     , ('ﾁ', "ヂ")
                     , ('ﾂ', "ヅ")
                     , ('ﾃ', "デ")
                     , ('ﾄ', "ド")
                     , ('ﾊ', "バ")
                     , ('ﾋ', "ビ")
                     , ('ﾌ', "ブ")
                     , ('ﾍ', "ベ")
                     , ('ﾎ', "ボ")
                     , ('ｳ', "ヴ")
                     ]

kanaVerseMap :: M.Map Char Char
kanaVerseMap = M.fromList [ ('ガ', 'ｶ')
                          , ('ギ', 'ｷ')
                          , ('グ', 'ｸ')
                          , ('ゲ', 'ｹ')
                          , ('ゴ', 'ｺ')
                          , ('ザ', 'ｻ')
                          , ('ジ', 'ｼ')
                          , ('ズ', 'ｽ')
                          , ('ゼ', 'ｾ')
                          , ('ゾ', 'ｿ')
                          , ('ダ', 'ﾀ')
                          , ('ヂ', 'ﾁ')
                          , ('ヅ', 'ﾂ')
                          , ('デ', 'ﾃ')
                          , ('ド', 'ﾄ')
                          , ('バ', 'ﾊ')
                          , ('ビ', 'ﾋ')
                          , ('ブ', 'ﾌ')
                          , ('ベ', 'ﾍ')
                          , ('ボ', 'ﾎ')
                          , ('ヴ', 'ｳ')]

kanaMap2 = M.fromList [ ('ﾊ', "パ")
                      , ('ﾋ', "ピ")
                      , ('ﾌ', "プ")
                      , ('ﾍ', "ペ")
                      , ('ﾎ', "ポ")
                      ]

kanaVerseMap2 :: M.Map Char Char
kanaVerseMap2 = M.fromList [ ('パ', 'ﾊ')
                           , ('ピ', 'ﾋ')
                           , ('プ', 'ﾌ')
                           , ('ペ', 'ﾍ')
                           , ('ポ', 'ﾎ')
                           ]
----------------------------------------------------------------------------------------------------
preStr    = concat [preAlnum, preKigou, preKana]
postStr   = concat [postAlnum, postKigou, postKana]
----------------------------------------------------------------------------------------------------
charMap, charVerseMap :: M.Map Char Char
charMap      = M.fromList $ zip preStr postStr
charVerseMap = M.fromList $ zip postStr preStr

toZenkaku, toHankaku :: String -> String
toZenkaku = reverse . _toZenkaku . reverse

_toZenkaku :: [Char] -> [Char]
_toZenkaku "" = ""
_toZenkaku [x] = [fromMaybe x (M.lookup x charMap)]
_toZenkaku (x:y:xs)
  | x == 'ﾞ' = fromMaybe [y] (M.lookup y kanaMap) ++ _toZenkaku xs
  | x == 'ﾟ' = fromMaybe [y] (M.lookup y kanaMap2) ++ _toZenkaku xs
  | otherwise =
    fromMaybe x (M.lookup x charMap) : _toZenkaku (y:xs)

toHankaku "" = ""
-- toHankaku (x:xs) = fromMaybe x (M.lookup x charVerseMap) : toHankaku xs
toHankaku (x:xs) = headString ++ toHankaku xs
  where
    headString = ((:[]) x) `fromMaybe`
                   ((:[]) <$> x `M.lookup` charVerseMap
                    <|> (: ['ﾞ']) <$> x `M.lookup` kanaVerseMap
                    <|> (: ['ﾟ']) <$> x `M.lookup` kanaVerseMap2)
----------------------------------------------------------------------------------------------------
toZenkakuSpec :: Spec
toZenkakuSpec = do
  describe "toZenkaku" $ do
    it "01" $ toZenkaku "ｱｲｽｸﾘ-ﾑ"    `shouldBe` "アイスクリーム"
    it "02" $ toZenkaku "ｶﾞﾘｯﾄﾁｭｳ"   `shouldBe` "ガリットチュウ"
    it "03" $ toZenkaku "ﾋﾟﾝﾎﾟﾝ"     `shouldBe` "ピンポン"
    it "04" $ toZenkaku "archive"    `shouldBe` "ａｒｃｈｉｖｅ"
    it "05" $ toZenkaku "attend"     `shouldBe` "ａｔｔｅｎｄ"
    it "06" $ toZenkaku "ad hoc"     `shouldBe` "ａｄ　ｈｏｃ"
    it "07" $ toZenkaku "inter-bank" `shouldBe` "ｉｎｔｅｒーｂａｎｋ"
    it "08" $ toZenkaku "1234567"    `shouldBe` "１２３４５６７"
    it "09" $ toZenkaku "ﾎ1ｹﾞ2ﾝ34"   `shouldBe` "ホ１ゲ２ン３４"
    it "10" $ toZenkaku "ほ@%&()!"   `shouldBe` "ほ＠％＆（）！"
    it "11" $ toZenkaku "$=<>?_+"    `shouldBe` "＄＝＜＞？＿＋"
    it "12" $ toZenkaku "V1~5"       `shouldBe` "Ｖ１～５"
    it "13" $ toZenkaku ""           `shouldBe` ""
    it "14" $ toZenkaku " "          `shouldBe` "　"
    it "15" $ toZenkaku "久世中久町705-1 ﾙﾐｴｰﾙ桂川301号" `shouldBe` "久世中久町７０５ー１　ルミエール桂川３０１号"

toHankakuSpec :: Spec
toHankakuSpec = do
  describe "toHankaku" $ do
    it "01" $ toHankaku "アイスクリーム"       `shouldBe` "ｱｲｽｸﾘｰﾑ"
    it "02" $ toHankaku "アドホック"           `shouldBe` "ｱﾄﾞﾎｯｸ"
    it "03" $ toHankaku "アーキテクチャ"       `shouldBe` "ｱｰｷﾃｸﾁｬ"
    it "04" $ toHankaku "アカウンタビリティー" `shouldBe` "ｱｶｳﾝﾀﾋﾞﾘﾃｨ"
    it "05" $ toHankaku "イデオロギー"         `shouldBe` "ｲﾃﾞｵﾛｷﾞｰ"
    it "06" $ toHankaku "ガリットチュウ"       `shouldBe` "ｶﾞﾘｯﾄﾁｭｳ"
    it "07" $ toHankaku "ピンポン"             `shouldBe` "ﾋﾟﾝﾎﾟﾝ"
    it "08" $ toHankaku "ａｒｃｈｉｖｅ"       `shouldBe` "archive"
    it "09" $ toHankaku "ａｔｔｅｎｄ"         `shouldBe` "attend"
    it "10" $ toHankaku "ａｄ　ｈｏｃ"         `shouldBe` "ad hoc"
    it "11" $ toHankaku "ｉｎｔｅｒーｂａｎｋ" `shouldBe` "inter-bank"
    it "12" $ toHankaku "１２３４５６７"       `shouldBe` "1234567"
    it "13" $ toHankaku "ホ１ゲ２ン３４"       `shouldBe` "ﾎ1ｹﾞ2ﾝ34"
    it "14" $ toHankaku "ほ＠％＆（）！"       `shouldBe` "ほ@%&()!"
    it "15" $ toHankaku "＄＝＜＞？＿＋"       `shouldBe` "$=<>?_+"
    it "16" $ toHankaku "Ｖ１ー５"             `shouldBe` "V1~5"
    it "17" $ toHankaku ""                     `shouldBe` ""
    it "18" $ toHankaku "　"                   `shouldBe` " "    

test :: IO ()
test = do
  I.hSetEncoding I.stdout I.utf8

  I.putStrLn $ toHankaku "アーキテクチャ"
  I.putStrLn $ (:[]) $ 'ガ'
