module KensinConfig
    ( Config (..)
    , config
    ) where

import Data.Array

data Config = Con { file       :: FilePath
                  , excelFile  :: FilePath
                  , rubyProg   :: FilePath
                  , keyColNum  :: Int
                  , year       :: Integer
                  , vArray     :: Array Int (Integer, Integer)
                  , extract    :: [(Int, String)]}

config = Con { file       = "f:/Haskell/.kensin"
             , excelFile  = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
             , rubyProg   = "f:/Haskell/kensin/kensin.rb"
             , keyColNum  = 12
             , year       = 2016
             , vArray     =
               listArray (1, 14) [ (6000, 3000) -- 胃バリウム
                                 , (2500, 2500) -- 腹部エコー
                                 , (8500, 5500) -- 胃バリウム + 腹部エコー
                                 , (1500, 1500) -- 肝炎ウィルス
                                 , (1500,  500) -- ペプシノーゲン
                                 , (1500,  500) -- ピロリ菌
                                 , (1500,  500) -- 前立腺がん
                                 , (5000,    0) -- 乳がん・マンモ
                                 , (5000,    0) -- 乳がん・乳腺エコー
                                 , (3000, 2000) -- 子宮がん
                                 , (8000, 4000) -- 胃カメラ
                                 , (1000, 1000) -- 骨密度
                                 , (3500, 3500) -- アスベスト
                                 , (3500, 3500) -- じん肺
                                 ]
             , extract     = [ (0, "氏名")
                             , (4, "性別")
                             , (5, "生年月日")
                             , (10, "保険証番号")
                             , (9, "区分")
                             , (18, "補助")
                             , (19, "日時")
                             , (20, "申込日時")
                             , (21, "無料オプション")
                             , (22, "有料オプション")]
             }
