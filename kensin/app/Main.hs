module Main where

import Util                             ((&&&))
import Util.StrEnum                     (split)
import Data.Maybe                       (fromMaybe)
import Data.Time
import Data.List                        (findIndex)
import System.Process
import qualified System.IO              as I

-- 受診券整理番号,分会名,,氏名,フリガナ2,フリガナ1,フリガナ,性,生年月日,年,被保証記号,被保証番号,本人家族,記号番号,区,住所,郵便番号,電話番号,備考１,備考２,個人番号,コース名,受診日時,申込,無料オプション,追加オプション,,,

data Config = Config { titleList :: [String] }

config = Config { titleList = ["受診券整理番号"
                              , "分会名"
                              , ""
                              , "氏名"
                              , "フリガナ2"
                              , "フリガナ1"
                              , "フリガナ"
                              , "性"
                              , "生年月日"
                              , "年"
                              , "被保証記号"
                              , "被保証番号"
                              , "本人家族"
                              , "記号番号"
                              , "区"
                              , "住所"
                              , "郵便番号"
                              , "電話番号"
                              , "備考１"
                              , "備考２"
                              , "個人番号"
                              , "コース名"
                              , "受診日時"
                              , "申込"
                              , "無料オプション"
                              , "追加オプション"]}

data Gender = Male | Female deriving (Show, Eq)
data Status = Already | Yet deriving (Show, Eq)
data Kind   = H | K deriving (Show, Eq)
data KensinData = KensinData { day     :: Maybe Day,
                               sortKey :: Maybe String,
                               name    :: String, 
                               gender  :: Gender,
                               old     :: Maybe Integer, 
                               number  :: Maybe String,
                               kind    :: Kind,
                               stat    :: Status,
                               kday    :: String, 
                               amount  :: Maybe Integer,
                               key     :: Maybe (Day, Integer, Integer),
                               pay     :: Maybe [String],
                               nonPay  :: Maybe [String] } deriving (Show, Eq)

instance Ord KensinData where
  compare (KensinData _ x _ _ _ _ _ _ _ _ _ _ _) (KensinData _ y _ _ _ _ _ _ _ _ _ _ _)
    | x > y = GT
    | x == y = EQ
    | otherwise = LT

symKeyword = "受診券整理番号"
optJ       = "無料オプション"
opt        = "1・2・3・4・5・6・7・8・9"

type OptColumn = Int

column :: String -> [String] -> Int
column key target =
  fromMaybe 100 $ findIndex (==key) target

filtering :: OptColumn -> [[String]] -> [[String]]
filtering colNum = filter (notSymKey &&& notOpt)
  where notSymKey = (/= symKeyword) . (!! 0)
        notOpt    = (/= opt) . (!! colNum)

runRuby :: IO (I.Handle, I.Handle, I.Handle, ProcessHandle)
runRuby = runInteractiveProcess "ruby" ["f:/Haskell/kensin/kensin.rb"] Nothing Nothing

main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  (_, sout, _, _) <- runRuby
  cont <- lines <$> I.hGetContents sout
  -- let mainFilter = filtering (column optJ $ head cont) . map (split ',')
  -- mapM_ (putStrLn . concat) $ mainFilter cont
  putStrLn $ head cont
