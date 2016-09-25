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
-- [ n                     -- (0) 名前
--   , _                   -- (1) フリガナ1
--   , _                   -- (2) フリガナ2
--   , _                   -- (3) フリガナ3
--   , g                   -- (4) 性別
--   , birth               -- (5) 誕生日
--   , _                   -- (6) 年齢
--   , _                   -- (7) 保険証記号
--   , num                 -- (8) 保険証番号
--   , _                   -- (9) 本人/家族
--   , _                   -- (10) 保険証記号番号
--   , k                   -- (11) 本/家
--   , _                   -- (12) 住所
--   , _                   -- (13) 郵便番号
--   , _                   -- (14) 電話
--   , _                   -- (15) 組合員番号
--   , _                   -- (16) ？
--   , _                   -- (17) 世帯番号
--   , st                  -- (18) 受診フラグ
--   , day'                -- (19) 受付日時
--   , kday'               -- (20) 受診日時
--   , nop                 -- (21) 無料オプション
--   , op                  -- (22) 有料オプション
--   ]
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
