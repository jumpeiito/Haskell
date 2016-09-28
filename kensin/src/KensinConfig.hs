module KensinConfig
    ( Config (..)
    , config
    ) where

import Data.Array
import Data.Time

data Config = Con { file       :: FilePath
                  , excelFile  :: FilePath
                  , rubyProg   :: FilePath
                  , keyColNum  :: Int
                  , year       :: Integer
                  , vArray     :: Array Int (Integer, Integer)
                  , extract    :: [(Int, String)]
                  , bkArray    :: Array Int String
                  , sunday     :: Day
                  }

config = Con { file       = "f:/Haskell/.kensin"
             , excelFile  = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
             , rubyProg   = "f:/Haskell/kensin/kensin.rb"
             , keyColNum  = 12
             , year       = 2016
             , vArray     =
               listArray (1, 14) [ (6000, 3000) -- (1)胃バリウム
                                 , (2500, 2500) -- (2)腹部エコー
                                 , (8500, 5500) -- (3)胃バリウム + 腹部エコー
                                 , (1500, 1500) -- (4)肝炎ウィルス
                                 , (1500,  500) -- (5)ペプシノーゲン
                                 , (1500,  500) -- (6)ピロリ菌
                                 , (1500,  500) -- (7)前立腺がん
                                 , (5000,    0) -- (8)乳がん・マンモ
                                 , (5000,    0) -- (9)乳がん・乳腺エコー
                                 , (3000, 2000) -- (10)子宮がん
                                 , (8000, 4000) -- (11)胃カメラ
                                 , (1000, 1000) -- (12)骨密度
                                 , (3500, 3500) -- (13)アスベスト
                                 , (3500, 3500) -- (14)じん肺
                                 ]
             , extract     = [ (0, "分会")
                             , (2, "氏名")
                             , (6, "性別")
                             , (7, "生年月日")
                             , (12, "保険証番号")
                             , (11, "区分")
                             , (20, "補助")
                             , (21, "日時")
                             , (22, "申込日時")
                             , (23, "無料オプション")
                             , (24, "有料オプション")]
             , bkArray     =
               listArray (0, 5) ["石田", "日野", "小栗栖", "一言寺", "三宝院", "点在"]
             , sunday      = fromGregorian 2016 4 17
             }
