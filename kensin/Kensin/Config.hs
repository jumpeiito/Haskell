{-# LANGUAGE FlexibleContexts #-}

module Kensin.Config
    ( Gender (..)
    , YearClass (..)
    , Config (..)
    , KensinOption (..)
    , config
    ) where

import Control.Monad.Reader
import Data.Array
import Data.Time

data Gender = Male | Female deriving (Show, Eq)
data YearClass = Under Gender | Over Gender deriving (Show, Eq)
data KensinOption =
  Pylori                        -- ピロリ菌
  | Fundus                      -- 眼底検査
  | Pepsinogen                  -- ペプシノーゲン
  | Barium                      -- 胃バリウム
  | PSA                         -- 前立腺
  | MammaryGlandEcho            -- 乳腺エコー
  | Mammography                 -- マンモグラフィー
  | Uterine                     -- 子宮がん
  | AbdominalEcho               -- 腹部エコー
  | HepatitisVirus              -- 肝炎ウィルス
  | Gastroscope                 -- 胃カメラ
  | BoneDensity                 -- 骨密度
  | Asbestos                    -- アスベスト
  | Pneumoconiosis              -- じん肺
  | AB                          -- 腹部エコー+胃バリウム
  deriving (Show, Eq)

data Config = Con { file               :: FilePath
                  , excelFile          :: FilePath
                  , rubyProg           :: FilePath
                  , keyColNum          :: Int
                  , year               :: Integer
                  , nonPayList         :: [([YearClass], [KensinOption])]
                  , payList            :: [(KensinOption, (Integer, Integer))]
                  , vArray             :: Array Int (Integer, Integer)
                  , extract            :: [(Int, String)]
                  , bkArray            :: Array Int String
                  , sunday             :: Day
                  , receiptCommand     :: String
                  , receiptEnvironment :: String
                  , meiboCommand       :: String
                  , meiboEnvironment   :: String
                  , meiboLength        :: Int
                  , optionLadies1      :: [KensinOption]
                  , optionLadies2      :: [KensinOption]
                  , optionCamera       :: [KensinOption]
                  , optionJinpai       :: [KensinOption]
                  }

config = Con { file       = "f:/Haskell/.kensin"
             , excelFile  = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
             , rubyProg   = "f:/Haskell/kensin/kensin.rb"
             , keyColNum  = 12
             , year       = 2016
             , nonPayList = [ ( [Under Male, Under Female]
                              , [Pylori] )
                            , ( [Over Male, Over Female]
                              , [Fundus, Pylori, Pepsinogen])
                            , ( [Over Male, Over Female]
                              , [Fundus, Barium])
                            , ( [Over Male]
                              , [Fundus, Pylori, PSA])
                            , ( [Over Female]
                              , [Fundus, Pylori, Pepsinogen, MammaryGlandEcho])
                            , ( [Over Female]
                              , [Fundus, Pylori, Pepsinogen, Mammography])
                            , ( [Over Female]
                              , [Fundus, Uterine])
                            , ( [Over Female]
                              , [Fundus, Uterine, MammaryGlandEcho])
                            , ( [Over Female]
                              , [Fundus, Uterine, Mammography])]
             , payList    = [ (Barium,           (6000, 3000)) -- (1)胃バリウム
                            , (AbdominalEcho,    (2500, 2500)) -- (2)腹部エコー
                            , (AB,               (8500, 5500)) -- (3)胃バリウム + 腹部エコー
                            , (HepatitisVirus,   (1500, 1500)) -- (4)肝炎ウィルス
                            , (Pepsinogen,       (1500,  500)) -- (5)ペプシノーゲン
                            , (Pylori,           (1500,  500)) -- (6)ピロリ菌
                            , (PSA,              (1500,  500)) -- (7)前立腺がん
                            , (Mammography,      (5000,    0)) -- (8)乳がん・マンモ
                            , (MammaryGlandEcho, (5000,    0)) -- (9)乳がん・乳腺エコー
                            , (Uterine,          (3000, 2000)) -- (10)子宮がん
                            , (Gastroscope,      (8000, 4000)) -- (11)胃カメラ
                            , (BoneDensity,      (1000, 1000)) -- (12)骨密度
                            , (Asbestos,         (3500, 3500)) -- (13)アスベスト
                            , (Pneumoconiosis,   (3500, 3500)) -- (14)じん肺
                            ]
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
                             , (5, "フリガナ")
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
             , sunday             = fromGregorian 2016 4 17
             , receiptCommand     = "writer"
             , receiptEnvironment = "receiptPage"
             , meiboCommand       = "meibo"
             , meiboEnvironment   = "SundayMeibo"
             , meiboLength        = 35
             , optionLadies1      = [MammaryGlandEcho, Mammography, Uterine]
             , optionLadies2      = [MammaryGlandEcho, Mammography]
             , optionJinpai       = [Asbestos, Pneumoconiosis]
             , optionCamera       = [Gastroscope]
             }



kensinPayList = [ (Barium,           (6000, 3000)) -- (1)胃バリウム
                , (AbdominalEcho,    (2500, 2500)) -- (2)腹部エコー
                , (AB,               (8500, 5500)) -- (3)胃バリウム + 腹部エコー
                , (HepatitisVirus,   (1500, 1500)) -- (4)肝炎ウィルス
                , (Pepsinogen,       (1500,  500)) -- (5)ペプシノーゲン
                , (Pylori,           (1500,  500)) -- (6)ピロリ菌
                , (PSA,              (1500,  500)) -- (7)前立腺がん
                , (Mammography,      (5000,    0)) -- (8)乳がん・マンモ
                , (MammaryGlandEcho, (5000,    0)) -- (9)乳がん・乳腺エコー
                , (Uterine,          (3000, 2000)) -- (10)子宮がん
                , (Gastroscope,      (8000, 4000)) -- (11)胃カメラ
                , (BoneDensity,      (1000, 1000)) -- (12)骨密度
                , (Asbestos,         (3500, 3500)) -- (13)アスベスト
                , (Pneumoconiosis,   (3500, 3500)) -- (14)じん肺
                ]

