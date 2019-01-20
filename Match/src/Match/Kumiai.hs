-- -*- coding: utf-8 -*-
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
module Match.Kumiai where

import           Control.Arrow               ((&&&))
import           Control.Lens                ((^.))
import qualified Data.Map.Strict             as M
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Extensible
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (foldl', map)
import qualified Data.Text                   as Tx
import           Data.Time                   (Day (..))
import           Match.SQL
import           Match.Base                  (killBlanks, killBunkai
                                             , killShibu, makeKey
                                             , officeTypeRegularize
                                             , regularize)
import           Text.Read                   (readMaybe)
import           Util.Strbt                  (strdt)
import           Util

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
makeSY "顧問"             = Just $ SY 11 "顧問"
makeSY _                  = Nothing

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

type Kumiai = Record
  '[ "shibuCode"  >: Text
   , "shibu"      >: Text
   , "bunkaiCode" >: Text
   , "bunkai"     >: Text
   , "han"        >: Text
   , "number"     >: Text
   , "name"       >: Text
   , "kana"       >: Text
   , "sex"        >: Text
   , "got"        >: Maybe Day
   , "lost"       >: Maybe Day
   , "birth"      >: Maybe Day
   , "work"       >: Text
   , "office"     >: Text
   , "officeCode" >: Text
   , "printOrder" >: Maybe Double
   , "phone"      >: Text
   , "cellPhone"  >: Text
   , "fax"        >: Text
   , "postal"     >: Text
   , "address"    >: Text
   , "rawAddress" >: Text
   , "kind"       >: Text
   , "kyousai"    >: Text
   , "honbuY"     >: Maybe HY
   , "shibuY"     >: Maybe [SY]
   , "bunkaiY"    >: Maybe [BY]
   , "hanY"       >: Maybe Text
   , "kokuhoGet"  >: Maybe Day
   , "kokuhoLost" >: Maybe Day
   , "relational" >: Maybe Text
   ]

kanaBirthKey :: Kumiai -> (Text, Maybe Day)
kanaBirthKey k = (killBlanks (k ^. #kana), k ^. #birth)

makeKumiai :: [Text] -> Kumiai
makeKumiai record' = case record' of
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
    , _officeCode               -- 就労先コード
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
    -> #shibuCode @= _sc
       <: #shibu      @= killShibu _s
       <: #bunkaiCode @= _bc
       <: #bunkai     @= killBunkai _b
       <: #han        @= _h
       <: #number     @= makeKey 7 _num
       <: #name       @= _name
       <: #kana       @= (regularize $ killBlanks _kana)
       <: #sex        @= _sex
       <: #got        @= strdt _got
       <: #lost       @= (if _lost == "" then Nothing else strdt _lost)
       <: #birth      @= strdt _birth
       <: #work       @= (officeTypeRegularize $ killBlanks _work)
       <: #office     @= _office
       <: #officeCode @= _officeCode
       <: #printOrder @= (readMaybe $ Tx.unpack _printOrder)
       <: #phone      @= _tel
       <: #cellPhone  @= _cellphone
       <: #fax        @= _fax
       <: #postal     @= _postal
       <: #address    @= _address
       <: #rawAddress @= killBlanks _address
       <: #kind       @= _kind
       <: #kyousai    @= _kyosai
       <: #honbuY     @= makeHY _honbuY
       <: #shibuY     @= makeSYList _shibuY
       <: #bunkaiY    @= makeBYList _bunkaiY
       <: #hanY       @= blankMaybe _hanY
       <: #kokuhoGet  @= strdt _kokuhoGet
       <: #kokuhoLost @= strdt _kokuhoLost
       <: #relational @= Nothing
       <: nil
  _ -> error $ "must not be happen." <> (Tx.unpack $ Tx.intercalate (Tx.pack ",") record')

-- |
--
-- >>> blankMaybe ""
-- Nothing
-- >>> blankMaybe "text"
-- Just "text"
blankMaybe :: Text -> Maybe Text
blankMaybe "" = Nothing
blankMaybe x  = Just x

instance Sourceable Kumiai where
  source = SQLSource { specGetter    = #kumiaiSpec
                     , csvPathGetter = #kumiaiFile
                     , dbPathGetter  = #kumiaiDB
                     , makeFunction  = makeKumiai }

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
  let k = kumiaiMakeKey . (^. #number)
  initializeList ===> Key k `MakeSingletonMap` Value id

birthdayMap :: IO (M.Map (Maybe Day) [Kumiai])
birthdayMap = do
  initializeList ===>
    Key (^. #birth) `MakeListMap` Value id

birthdayNameMap :: IO (M.Map (Text, Maybe Day) [Kumiai])
birthdayNameMap = do
  let toKey el = (killBlanks $ el ^. #kana, el ^. #birth)
  initializeList ===>
    Key toKey `MakeListMap` Value id

numberCMap :: IO (M.Map Text Kumiai)
numberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((kumiaiMakeKey . (^. #number)) &&& id)
              $$ CL.consume)

birthdayCMap :: IO (M.Map (Maybe Day) [Kumiai])
birthdayCMap = do
  let insert mp el =
        M.insertWith (++) (el ^. #birth) [el] mp
  initializeSource $$ CL.fold insert M.empty

birthdayNameCMap :: IO (M.Map (Text, Maybe Day) [Kumiai])
birthdayNameCMap = do
  let insert mp el =
        let b = el ^. #birth
        in let k = killBlanks $ el ^. #kana
        in M.insertWith (++) (k, b) [el] mp
  initializeSource $$ CL.fold insert M.empty

officeCodeMap :: IO (M.Map Text [Kumiai])
officeCodeMap = do
  ls <- runConduit
        $ initializeSource
        .| CL.consume
  let rize = Tx.justifyRight 7 '0'
  return $
    ls ==> Key (rize . (^. #officeCode)) `MakeListMap` Value id

verboseName :: Kumiai -> Text
verboseName k = k ^. #name <> "(" <> k ^. #kana <> ")"
