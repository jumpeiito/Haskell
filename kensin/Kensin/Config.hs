{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Kensin.Config
    ( Gender (..)
    , YearClass (..)
    , Config (..)
    , KensinOption (..)
    , ShowDirector (..)
    , config
    ) where

import Control.Monad.Reader
import Data.Array
import Data.Time
import System.IO                       (TextEncoding, utf8, mkTextEncoding)
import System.Environment
import Data.Yaml                        hiding (Parser, Array)
import GHC.Generics

data Gender = Male | Female deriving (Show, Eq)
data YearClass = Under Gender | Over Gender deriving (Show, Eq)
data KensinOption =
  Gemba                         -- 現場対応コース
  | Pylori                      -- ピロリ菌
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

data ShowDirector =
  DayStr Char
  | MonthDay Char
  | Bunkai
  | BunkaiHead
  | Year
  | Name
  | Amount
  | Furigana
  | Time
  | Paylist
  | Nonpaylist
  | Space
  | Tab
  deriving (Show, Eq, Read)

sjis :: IO TextEncoding
sjis = mkTextEncoding "CP932"

homeDirectory :: IO FilePath
homeDirectory = getEnv "home"

data Config = Con { file               :: FilePath
                  , rcFile             :: IO FilePath
                  , encoding           :: IO TextEncoding
                  , excelFile          :: FilePath
                  , outputCSV          :: FilePath
                  , rubyProg           :: FilePath
                  , keyColNum          :: Int
                  , year               :: Integer
                  , nonPayList         :: [([YearClass], [KensinOption])]
                  , payList            :: [(KensinOption, (Integer, Integer))]
                  , vArray             :: Array Int (Integer, Integer)
                  , extract            :: [(Int, String)]
                  , sunday             :: Day
                  , receiptCommand     :: String
                  , receiptEnvironment :: String
                  , receiptDirector    :: [ShowDirector]
                  , receiptLength      :: Int
                  , meiboCommand       :: String
                  , meiboEnvironment   :: String
                  , meiboDirector      :: [ShowDirector]
                  , meiboLength        :: Int
                  , optionLadies1      :: [KensinOption]
                  , optionLadies2      :: [KensinOption]
                  , optionCamera       :: [KensinOption]
                  , optionJinpai       :: [KensinOption]
                  , optionDirector     :: [ShowDirector]
                  } 

defaultConfig = Con { file       = "f:/Haskell/kensin/test.csv"
                    , rcFile     = (++ ".kensinrc") <$> homeDirectory
                    , encoding   = return utf8
                    , excelFile  = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
                    , outputCSV  = "f:/Haskell/kensin/output"
                    , rubyProg   = "f:/Haskell/kensin/kensin.rb"
                    , keyColNum  = 12
                    , year       = 2016
                    , nonPayList = [ ([], [])
                                   , ( [Under Male, Under Female]
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
                    , payList    = [ (Gemba,            (5400, 5400)) -- (0)現場対応コース
                                   , (Barium,           (6000, 3000)) -- (1)胃バリウム
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
                      listArray (0, 15) [ (5400, 5400) -- (0)現場対応コース
                                        , (6000, 3000) -- (1)胃バリウム
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
                    , sunday             = fromGregorian 2016 4 17
                    , receiptCommand     = "writer"
                    , receiptEnvironment = "receiptPage"
                    , receiptDirector    = [DayStr '/', Bunkai, Name, Amount]
                    , receiptLength      = 5
                    , meiboCommand       = "meibo"
                    , meiboEnvironment   = "SundayMeibo"
                    , meiboDirector      = [Name, Furigana, Time, Nonpaylist, Paylist, Amount]
                    , meiboLength        = 35
                    , optionLadies1      = [MammaryGlandEcho, Mammography, Uterine] -- 女性がん健診
                    , optionLadies2      = [MammaryGlandEcho, Mammography] -- 乳がん健診
                    , optionJinpai       = [Asbestos, Pneumoconiosis] -- アスベスト・じん肺
                    , optionCamera       = [Gastroscope]              -- 胃カメラ
                    , optionDirector     = [ MonthDay '-', Space
                                           , Time, Space
                                           , BunkaiHead, Tab
                                           , Name, Tab
                                           , Year, Space
                                           , Amount, Space
                                           , Nonpaylist, Space
                                           , Paylist ]
                    }
----------------------------------------------------------------------------------------------------
data MyConfig = MC { myEncoding      :: String
                   , myExcelFile     :: String
                   , myOutputCSV     :: String
                   , myYear          :: Integer
                   , mySunday        :: Day
                   , myReceiptLength :: Int
                   , myMeiboLength   :: Int
                   } deriving (Show)

instance FromJSON MyConfig where
  parseJSON (Object v) = MC <$> v .: "Encoding"
                            <*> v .: "ExcelFile"
                            <*> v .: "CSV"
                            <*> v .: "Year"
                            <*> v .: "Sunday"
                            <*> v .: "ReceiptLength"
                            <*> v .: "MeiboLength"

encodeString :: String -> IO TextEncoding
encodeString s | s == "utf8" = return utf8
               | s == "sjis" = sjis

config :: IO Config
config = do
  home <- homeDirectory
  Just rc <- decodeFile (home ++ "/.kensinrc")
  return $ defaultConfig { encoding       = encodeString $ myEncoding rc
                         , excelFile      = myExcelFile rc
                         , outputCSV      = myOutputCSV rc
                         , year           = myYear rc
                         , sunday         = mySunday rc
                         , receiptLength  = myReceiptLength rc
                         , meiboLength    = myMeiboLength rc
                         }
