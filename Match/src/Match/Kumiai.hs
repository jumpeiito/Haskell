{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Match.Kumiai where

import           Control.Arrow
import           Control.Parallel.Strategies
import qualified Data.Map.Strict             as M
import           Data.List                   (foldl')
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (foldl', map)
import qualified Data.Text                   as Tx
import           Data.Time                   (Day (..))
import           GHC.Generics
import           Match.Config                ( Config (..)
                                             , kumiaiSpecF)
import           Match.SQL                   ( fetchSQL
                                             , fetchSQLSource)
import           Match.Base                  (killBlanks, killBunkai
                                             , killShibu, makeKey
                                             , officeTypeRegularize
                                             , regularize)
import           Text.Read                   (readMaybe)
import           Util.Strbt                  (strdt)

type MaybeIO a = IO (Either String a)

data HY = HY Int String deriving (Show, Eq)
data SY = SY Int String deriving (Show, Eq)
data BY = BY Int String deriving (Show, Eq)
data HC = HC Int String deriving (Show, Eq)

instance Ord HY where
  compare (HY x s1) (HY y s2)
    | x == y && x == 11 = s1 `compare` s2
    | otherwise = x `compare` y

instance Ord SY where
  compare (SY x s1) (SY y s2)
    | x == y && x == 11 = s1 `compare` s2
    | otherwise = x `compare` y

instance Ord BY where
  compare (BY x s1) (BY y s2)
    | x == y && x == 11 = s1 `compare` s2
    | otherwise = x `compare` y

instance Ord HC where
  compare (HC x s1) (HC y s2)
    | x == y && x == 11 = s1 `compare` s2
    | otherwise = x `compare` y

makeHY :: Text -> Maybe HY
makeHY "執行委員長"       = Just $ HY 0 "執行委員長"
makeHY "副執行委員長"     = Just $ HY 1 "副執行委員長"
makeHY "本部専門部長"     = Just $ HY 2 "本部専門部長"
makeHY "本部書記長"       = Just $ HY 3 "本部書記長"
makeHY "本部書記次長"     = Just $ HY 4 "本部書記次長"
makeHY "常駐執行委員"     = Just $ HY 5 "常駐執行委員"
makeHY "本部執行委員"     = Just $ HY 6 "本部執行委員"
makeHY "特別執行委員"     = Just $ HY 8 "特別執行委員"
makeHY "本部主婦の会会長" = Just $ HY 9 "本部主婦の会会長"
makeHY "本部青年部長"     = Just $ HY 10 "本部青年部長"
makeHY "本部会計監査"     = Just $ HY 11 "本部会計監査"
makeHY _ = Nothing

hyToString :: HY -> Text
hyToString (HY _ tx) = Tx.pack tx

makeSY :: Text -> Maybe SY
makeSY "支部長"           = Just $ SY 0 "支部長"
makeSY "副支部長"         = Just $ SY 1 "副支部長"
makeSY "書記長"           = Just $ SY 2 "書記長"
makeSY "書記次長"         = Just $ SY 3 "書記次長"
makeSY "労働対策部長"     = Just $ SY 4 "労働対策部長"
makeSY "賃金対策部長"     = Just $ SY 4 "賃金対策部長"
makeSY "社会保障対策部長" = Just $ SY 4 "社会保障対策部長"
makeSY "税金対策部長"     = Just $ SY 4 "税金対策部長"
makeSY "自治体平和部長"   = Just $ SY 4 "自治体平和部長"
makeSY "組織部長"         = Just $ SY 4 "組織部長"
makeSY "財政部長"         = Just $ SY 4 "財政部長"
makeSY "文化厚生部長"     = Just $ SY 4 "文化厚生部長"
makeSY "教育宣伝部長"     = Just $ SY 4 "教育宣伝部長"
makeSY "技住対部長"       = Just $ SY 4 "技住対部長"
makeSY "支部執行委員"     = Just $ SY 5 "支部執行委員"
makeSY "シニアの会会長"   = Just $ SY 6 "シニアの会会長"
makeSY "青年部長"         = Just $ SY 6 "青年部長"
makeSY "会計監査"         = Just $ SY 7 "会計監査"
makeSY "次世代建設委員"   = Just $ SY 8 "次世代建設委員"
makeSY "共済運営委員"     = Just $ SY 9 "共済運営委員"
makeSY "健康推進委員"     = Just $ SY 10 "健康推進委員"
makeSY "顧問"     = Just $ SY 11 "顧問"
makeSY _ = Nothing

makeSYList :: Text -> Maybe [SY]
makeSYList = mapM makeSY . Tx.splitOn "・"

syToString :: [SY] -> Text
syToString sy = Tx.intercalate "・" $ map toString sy
  where
    toString (SY _ s) = Tx.pack s

makeBY :: Text -> Maybe BY
makeBY "分会長" = Just $ BY 0 "分会長"
makeBY "副分会長" = Just $ BY 1 "副分会長"
makeBY "分会書記長" = Just $ BY 2 "分会書記長"
makeBY "分会会計" = Just $ BY 2 "分会会計"
makeBY "分会書記次長" = Just $ BY 3 "分会書記次長"
makeBY "分会執行委員" = Just $ BY 4 "分会執行委員"
makeBY "分会健康推進委員" = Just $ BY 5 "分会健康推進委員"
makeBY _ = Nothing

makeBYList :: Text -> Maybe [BY]
makeBYList = mapM makeBY . Tx.splitOn "・"

byToString :: [BY] -> Text
byToString by = Tx.intercalate "・" $ map toString by
  where
    toString (BY _ s) = Tx.pack s

makeHC :: Text -> Maybe HC
makeHC "班長" = Just $ HC 0 "班長"
makeHC _ = Nothing

hcToString :: HC -> Text
hcToString _ = "班長"

data Kumiai = K {
  kShibuCode    :: ! Text
  , kShibu      :: ! Text
  , kBunkaiCode :: ! Text
  , kBunkai     :: ! Text
  , kHan        :: ! Text
  , kNumber     :: ! Text
  , kName       :: ! Text
  , kKana       :: ! Text
  , kSex        :: ! Text
  , kGot        :: Maybe Day
  , kLost       :: Maybe Day
  , kBirthday   :: Maybe Day
  , kWork       :: ! Text
  , kOffice     :: ! Text
  , kPrintOrder :: Maybe Double
  , kPhone      :: ! Text
  , kCellPhone  :: ! Text
  , kFax        :: ! Text
  , kPostal     :: ! Text
  , kAddress    :: ! Text
  , kKind       :: ! Text
  , kKyousai    :: ! Text
  , kHonbuY     :: Maybe HY
  , kShibuY     :: Maybe [SY]
  , kBunkaiY    :: Maybe [BY]
  , kHanY       :: Maybe Text
  , kKokuhoGet  :: Maybe Day
  , kKokuhoLost :: Maybe Day
  , relational  :: Maybe Text
  } deriving (Show, Generic, Eq)

initializeCSVData :: IO [[Text]]
initializeCSVData = do
  spec <- kumiaiSpecF
  fetchSQL kumiaiFile spec kumiaiDB

initializeCSVSource :: Source IO [Text]
initializeCSVSource = do
  spec <- kumiaiSpecF
  fetchSQLSource kumiaiFile spec kumiaiDB

kanaBirthKey :: Kumiai -> (Text, Maybe Day)
kanaBirthKey k = (killBlanks $ kKana k, kBirthday k)

makeKumiai :: [Text] -> Kumiai
makeKumiai record = case record of
  [_sc                          -- 支部コード
    , _s                        -- 支部
    , _bc                       -- 分会コード
    , _b                        -- 分会
    , _h                        -- 班
    , _num                      -- 組合員番号
    , _name                     -- 氏名
    , _kana                     -- 氏名カナ
    , _sex                      -- 性別
    , _birth                    -- 生年月日
    , _got                      -- 加入日
    , _lost                     -- 脱退日
    , _work                     -- 職種
    , _office                   -- 就労先
    , _printOrder               -- 台帳表示順
    , _tel                      -- 電話番号
    , _cellphone                -- 携帯番号
    , _fax                      -- FAX
    , _postal                   -- 郵便番号
    , _address                  -- 住所
    , _kind                     -- 組合種別
    , _kyosai                   -- 共済区分
    , _honbuY                   -- 役職(本部)
    , _shibuY                   -- 役職(支部)
    , _bunkaiY                  -- 役職(分会)
    , _hanY                     -- 役職(班)
    , _kokuhoGet                -- 資格取得日
    , _kokuhoLost               -- 資格喪失日
    ]
    -> K { kShibuCode  = _sc
         , kShibu      = killShibu _s
         , kBunkaiCode = _bc
         , kBunkai     = killBunkai _b
         , kHan        = _h
         , kNumber     = makeKey 7 _num
         , kName       = _name
         , kKana       = regularize $ killBlanks _kana
         , kSex        = _sex
         , kGot        = strdt _got
         , kLost       = if _lost == "" then Nothing else strdt _lost
         , kBirthday   = strdt _birth
         , kWork       = officeTypeRegularize $ killBlanks _work
         , kOffice     = _office
         , kPrintOrder = readMaybe $ Tx.unpack _printOrder
         , kPhone      = _tel
         , kCellPhone  = _cellphone
         , kFax        = _fax
         , kPostal     = _postal
         , kAddress    = _address
         , kKind       = _kind
         , kKyousai    = _kyosai
         , kHonbuY     = makeHY _honbuY
         , kShibuY     = makeSYList _shibuY
         , kBunkaiY    = makeBYList _bunkaiY
         , kHanY       = blankMaybe _hanY
         , kKokuhoGet  = strdt _kokuhoGet
         , kKokuhoLost = strdt _kokuhoLost
         , relational  = Nothing
         }
  _ -> error $ "must not be happen." <> (Tx.unpack $ Tx.intercalate (Tx.pack ",") record)

blankMaybe :: Text -> Maybe Text
blankMaybe "" = Nothing
blankMaybe x  = Just x

initializeData :: IO [Kumiai]
initializeData = do
  csv <- initializeCSVData
  return $ parMap rseq makeKumiai csv

initializeSource :: Source IO Kumiai
initializeSource = initializeCSVSource $= CL.map makeKumiai

-- |
--
-- >>> Tx.unpack $ kumiaiMakeKey (Tx.pack "1234")
-- "000123"
-- >>> Tx.unpack $ kumiaiMakeKey (Tx.pack "1234567")
-- "123456"
kumiaiMakeKey :: Text -> Text
kumiaiMakeKey = Tx.take 6 . makeKey 7

numberMap :: IO (M.Map Text Kumiai)
numberMap = do
  csv <- initializeData
  return $
    M.fromList $ parMap rseq ((kumiaiMakeKey . kNumber) &&& id) csv

birthdayMap :: IO (M.Map (Maybe Day) [Kumiai])
birthdayMap = do
  csv <- initializeData
  let insert mp el =
        M.insertWith (++) (kBirthday el) [el] mp
  return $ foldl' insert M.empty csv

birthdayNameMap :: IO (M.Map (Text, Maybe Day) [Kumiai])
birthdayNameMap = do
  csv <- initializeData
  let insert mp el =
        let b = kBirthday el
        in let k = killBlanks $ kKana el
        in M.insertWith (++) (k, b) [el] mp
  return $ foldl' insert M.empty csv

numberCMap :: IO (M.Map Text Kumiai)
numberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((kumiaiMakeKey . kNumber) &&& id)
              $$ CL.consume)

birthdayCMap :: IO (M.Map (Maybe Day) [Kumiai])
birthdayCMap = do
  let insert mp el =
        M.insertWith (++) (kBirthday el) [el] mp
  initializeSource $$ CL.fold insert M.empty

birthdayNameCMap :: IO (M.Map (Text, Maybe Day) [Kumiai])
birthdayNameCMap = do
  let insert mp el =
        let b = kBirthday el
        in let k = killBlanks $ kKana el
        in M.insertWith (++) (k, b) [el] mp
  initializeSource $$ CL.fold insert M.empty

verboseName :: Kumiai -> Text
verboseName k = kName k <> "(" <> kKana k <> ")"
