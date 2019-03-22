{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Match.Base ( Office
                  , BaseInfo
                  , BaseInfoRecord
                  , DateString (..)
                  , killBlanks
                  , killShibu
                  , killBunkai
                  , killHyphen
                  , makeKey
                  , regularize
                  , officeTypeReplace
                  , officeTypeRegularize
                  , toCode
                  , toShibu
                  , killStringsAll
                  , katakanaP
                  ) where

import           Control.Applicative
import           Data.Extensible
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atp
import           Data.Either          (isRight)
import           Data.String
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as Tx
import           Data.Time            (Day (..))
import           Data.Tuple           (swap)
import           GHC.Generics

type Office = Record
  '[ "owner"       >: Text
   , "postal"      >: Text
   , "address"     >: Text
   , "address1"    >: Text
   , "address2"    >: Text
   , "tel"         >: Text
   , "fax"         >: Text
   , "got"         >: Maybe Day
   , "lost"        >: Maybe Day
   , "code"        >: Text
   , "name"        >: Text
   , "shibu"       >: Text
   , "otype"       >: Text
   , "rosaiCode"   >: Text
   , "rosaiNumber" >: Text
   , "koyouNumber" >: Text
   , "salaryEnd"   >: Text
   , "salaryPayM"  >: Text
   , "salaryPay"   >: Text
   , "hellowork"   >: Text
   ]

data BaseInfo =
  B { infoName  :: ! Text
    , infoKana  :: ! Text
    , infoBirth :: Maybe Day
    , rawBirth  :: Text } deriving (Show, Generic, Eq)

type BaseInfoRecord = Record
  '[ "name"     >: Text
   , "kana"     >: Text
   , "birth"    >: Maybe Day
   , "rawBirth" >: Text
   ]

-- $setup
-- >>> let pack = Tx.pack
-- >>> let unpack = Tx.unpack

-- |
-- >>> makeKey 7 (pack "1234")
-- "0001234"
-- >>> makeKey 7 (pack "1234567")
-- "1234567"
makeKey :: Int -> Text -> Text
makeKey = flip Tx.justifyRight '0'

-- |
-- >>> unpack $ killBlanks (pack " 123 456 7")
-- "1234567"
killBlanks :: Text -> Text
killBlanks = Tx.filter (`notElem` [' ', '　', '　'])

-- |
-- >>> killStringsParser (pack "hoge") `parseOnly` (pack "foohoge")
-- Right "foo"
-- >>> killStringsParser (pack "hoge") `parseOnly` (pack "hogefoo")
-- Right ""
-- >>> killStringsParser (pack "hoge") `parseOnly` (pack "foo")
-- Left "not enough input"
killStringsParser :: Text -> Parser String
killStringsParser s = manyTill anyChar (string s)

-- |
-- >>> unpack $ killBlanks (pack " 123 456 7  ")
-- "1234567"
-- >>> unpack $ killBlanks (pack "1234567")
-- "1234567"
killStrings :: Text -> Text -> Text
killStrings part s = case killStringsParser part `parseOnly` s of
                       Right xs -> Tx.pack xs
                       Left _   -> s

killStringsAllParser :: Text -> Parser String
killStringsAllParser s =
  (++)
    <$> manyTill anyChar (string s)
    <*> killStringsAllParser s
  <|> manyTill anyChar endOfInput

-- |
--
-- >>> :set -XOverloadedStrings
--
-- >>> killStringsAll "hoge" "hogefoo hogebuz"
-- "foo buz"
killStringsAll :: Text -> Text -> Text
killStringsAll part s = case killStringsAllParser part `parseOnly` s of
                          Right xs -> Tx.pack xs
                          Left _   -> s

-- |
-- >>> killHyphen "601-0000"
-- "6010000"
killShibu, killBunkai, killHyphen :: Text -> Text
killShibu  = killStrings "支部"
killBunkai = killStrings "分会"
killHyphen = Tx.filter (/= '-')


regularizeMap :: M.Map Char Char
regularizeMap = M.fromList [ ('ｧ', 'ｱ')
                           , ('ｨ', 'ｲ')
                           , ('ｩ', 'ｳ')
                           , ('ｪ', 'ｴ')
                           , ('ｫ', 'ｵ')
                           , ('ｶ', 'ｶ')
                           , ('ｹ', 'ｹ')
                           , ('ｯ', 'ﾂ')
                           , ('ｬ', 'ﾔ')
                           , ('ｭ', 'ﾕ')
                           , ('ｮ', 'ﾖ')
                           , ('ﾜ', 'ﾜ')]

regularize :: Text -> Text
regularize = Tx.map sconc
  where
    sconc el = fromMaybe el $ el `M.lookup` regularizeMap
-- --------------------------------------------------
-- |
-- >>> officeTypeReplace "株式会社青木工務店" == "㈱青木工務店"
-- True
officeTypeReplace :: Text -> Text
officeTypeReplace s = case otrParser `parseOnly` s of
                        Right xs -> mconcat xs
                        Left _   -> ""
  where
    otrParser = many (officeTypeReplaceParser <|> Atp.take 1)

-- |
-- >>> officeTypeRegularize "㈱青木工務店" == "株式会社青木工務店"
-- True
officeTypeRegularize :: Text -> Text
officeTypeRegularize s = either Tx.pack mconcat (otrParser `parseOnly` s)
  where
    otrParser = many (officeTypeRegularizeParser <|> Atp.take 1)

between :: Applicative f => f a1 -> f b -> f a -> f a
between op en c = op *> c <* en

-- |
-- >>> inParen (pack "hoge") `parseOnly` (pack "(hoge)")
-- Right "hoge"
inParen :: Text -> Parser Text
inParen s = between (satisfy $ inClass "(（") (satisfy $ inClass ")）") (string s)

class Shibu a where
  toCode  :: String -> a

instance Shibu (Maybe Int) where
  toCode = _toCode

instance Shibu (Maybe String) where
  toCode s = show <$> _toCode s

shibuCodeAlist :: [(String, Int)]
shibuCodeAlist = [ ("北",       10)
                 , ("上京",     11)
                 , ("京都中央",  12)
                 , ("中京",     12)
                 , ("下京",     13)
                 , ("南",       14)
                 , ("左京",     15)
                 , ("東山",     16)
                 , ("山科",     17)
                 , ("右京",     18)
                 , ("西京",     19)
                 , ("伏見",     20)
                 , ("醍醐",     21)
                 , ("乙訓",     50)
                 , ("宇治",     51)
                 , ("亀岡",     53)
                 , ("船井",     54)
                 , ("綾部",     56)
                 , ("福知山",   57)
                 , ("舞鶴",     58)
                 , ("宮津",     59)
                 , ("奥丹後",   60)
                 , ("相楽",     61)
                 , ("久世",     62)
                 , ("綴喜八幡", 63)]

officeTypeReplaceParser :: Parser Text
officeTypeReplaceParser =
  (string "株式会社" >> return "㈱")
  <|> (string "有限会社" >> return "㈲")
  <|> (inParen "株" >> return "㈱")
  <|> (inParen "有" >> return "㈲")

officeTypeRegularizeParser :: Parser Text
officeTypeRegularizeParser =
   (string "㈱" >> return "株式会社")
  <|> (string "㈲" >> return "有限会社")
  <|> (inParen "株" >> return "株式会社")
  <|> (inParen "有" >> return "有限会社")
  <|> (inParen "合" >> return "合同会社")

shibuCodeMap :: M.Map String Int
shibuCodeMap = M.fromList shibuCodeAlist

codeShibuMap :: M.Map Int String
codeShibuMap = M.fromList $ map swap shibuCodeAlist

-- |
-- >>> _toCode "北"
-- Just 10
_toCode :: String -> Maybe Int
_toCode s = s `M.lookup` shibuCodeMap

-- |
-- >>> toShibu 10 == Just "北"
-- True
toShibu :: Int -> Maybe String
toShibu i | i == 12 = Just "京都中央"
          | otherwise = i `M.lookup` codeShibuMap

inKatakana :: Char -> Bool
inKatakana = inClass ['ア'..'ン']

katakanaP :: Text -> Bool
katakanaP s = isRight parseResult
  where
    parseResult =
      many1 (satisfy inKatakana) `parseOnly` s

class (IsString a, Monoid a) => DateString a where
  translator :: String -> a
  dateToS :: Maybe Day -> a

  dateToS d = mempty `fromMaybe` (translator . show <$> d)

instance DateString String where
  translator = id

instance DateString Text where
  translator = Tx.pack
