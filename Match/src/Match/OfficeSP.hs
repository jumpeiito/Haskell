{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Match.OfficeSP where

import           Control.Applicative        ((<|>))
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as Tx
import           Data.Text                  hiding (foldl', map, count)
import           Data.Extensible
import           Data.Monoid
import           Data.Time                  ( Day (..))
import           Util
import           Util.Strbt                 (strdt)

type OfficeSP = Record
  '[ "code"      >: Text
   , "name"      >: Text
   , "nameKana"  >: Text
   , "owner"     >: Text
   , "ownerKana" >: Text
   , "shibu"     >: Text
   , "shibuName" >: Text
   , "hellowork" >: Text
   , "id"        >: Text
   , "groupid"   >: Text
   , "got"       >: Maybe Day
   , "lost"      >: Maybe Day
   , "kind"      >: Text
   , "kindP"     >: Text
   , "kindE"     >: Text
   , "otype"     >: Text
   , "payEnd"    >: Text
   , "payM"      >: Text
   , "payDay"    >: Text
   , "koyouNumber" >: Text
      ]

newtype OSP = KikanBango { runOSP :: OfficeSP }

instance Eq OSP where
  x == y =
    let ospq o =
          o ^. #otype <>
          o ^. #id <>
          (kikanBango o)
    in ospq (runOSP x) == ospq (runOSP y)

instance Ord OSP where
  x `compare` y =
    let edaban o =
          (o ^. #otype) <> (o ^. #id)
    in (kikanBango $ runOSP x) `compare` (kikanBango $ runOSP y) <>
       (edaban $ runOSP x) `compare` (edaban $ runOSP y)

makeOfficeSP :: [Text] -> OfficeSP
makeOfficeSP line' = case line' of
  [_c, _n, _nk, _o, _ok
    , _s, _sn, _hw, _i, _gi
    , _g, _l, _k, _kp, _ke, _otp
    , _pe, _pm, _pd, _kn]
   -> #code @= _c
      <: #name      @= _n
      <: #nameKana  @= _nk
      <: #owner     @= _o
      <: #ownerKana @= _ok
      <: #shibu     @= _s
      <: #shibuName @= _sn
      <: #hellowork @= _hw
      <: #id        @= _i
      <: #groupid   @= _gi
      <: #got       @= (strdt _g)
      <: #lost      @= (strdt _l)
      <: #kind      @= _k
      <: #kindP     @= _kp
      <: #kindE     @= _ke
      <: #otype     @= _otp
      <: #payEnd    @= (salaryDay _pe)
      <: #payM      @= (salaryMonth _pm)
      <: #payDay    @= (salaryDay _pd)
      <: #koyouNumber @= _kn
      <: nil
  _ -> error "must not be happen."

kikanBango :: OfficeSP -> Text
kikanBango o =
  Tx.take 5 $ takeEnd 6 $ o ^. #groupid

salaryDay :: Text -> Text
salaryDay "31" = "末"
salaryDay "0"  = ""
salaryDay s    = s

salaryMonth :: Text -> Text
salaryMonth "1" = "当月"
salaryMonth "2" = "翌月"
salaryMonth _   = ""

outofP :: OfficeSP -> Bool
outofP o | Tx.take 2 (o ^. #koyouNumber) == "26" = False
         | otherwise = True

koyouNumberDivide :: Text -> Maybe (Text, Text, Text)
koyouNumberDivide k =
  case knDParse `parseOnly` k of
    Right x -> Just x
    Left _  -> Nothing
  where
    knDParse = do
      h1 <- Tx.pack <$> count 4 digit
      h2 <- Tx.pack <$> count 6 digit
      h3 <- Tx.pack <$> count 1 digit
      return (h1, h2, h3)

koyouNumberHeader :: Text -> Text
koyouNumberHeader k =
  case koyouNumberDivide k of
    Nothing -> ""
    Just (h, _, _) -> h

koyouNumberRest :: Text -> Text
koyouNumberRest k =
  case koyouNumberDivide k of
    Nothing -> ""
    Just (_, r1, r2) -> r1 <> "-" <> r2

koyoP :: OfficeSP -> Bool
koyoP o | (o ^. #otype) == "2" = True
        | (o ^. #otype) == "0" && (o ^. #koyouNumber) /= "" = True
        | otherwise = False

rosaiNumberKey :: OfficeSP -> Text
rosaiNumberKey o =
  Tx.takeEnd 6 (o ^. #groupid) <>
  Tx.justifyRight 4 '0' (o ^. #id)

lostDayString :: OfficeSP -> Maybe Text
lostDayString o = Tx.pack . show <$> o ^. #lost

helloworkForNendo :: OfficeSP -> Maybe Text
helloworkForNendo o =
  if (outofP o)
  then Just (o ^. #hellowork)
  else Nothing

headerForNendo :: OfficeSP -> Text
headerForNendo o =
  mempty `fromMaybe` (lostDayString o <|> helloworkForNendo o)

outputForNendo :: OfficeSP -> Text
outputForNendo o = toCSV txs
  where
    txs = [ headerForNendo o
          , o ^. #shibu
          , o ^. #shibuName
          , toHelloWorkNumber (takeEnd 2 $ o ^. #shibu)
          , kikanBango o
          , o ^. #otype
          , o ^. #id
          , koyouNumberHeader (o ^. #koyouNumber)
          , koyouNumberRest (o ^. #koyouNumber)
          , o ^. #name
          , o ^. #payEnd
          , o ^. #payM <> o ^. #payDay
          ]

toHelloWork :: Text -> Maybe (Text, Text)
toHelloWork "10" = Just ("01", "西陣")
toHelloWork "11" = Just ("01", "西陣")
toHelloWork "12" = Just ("01", "西陣")
toHelloWork "13" = Just ("02", "七条")
toHelloWork "14" = Just ("02", "七条")
toHelloWork "15" = Just ("01", "西陣")
toHelloWork "16" = Just ("02", "七条")
toHelloWork "17" = Just ("02", "七条")
toHelloWork "18" = Just ("01", "西陣")
toHelloWork "19" = Just ("01", "西陣")
toHelloWork "20" = Just ("03", "伏見")
toHelloWork "21" = Just ("03", "伏見")
toHelloWork "50" = Just ("02", "七条")
toHelloWork "51" = Just ("08", "宇治")
toHelloWork "53" = Just ("01", "西陣")
toHelloWork "54" = Just ("01", "西陣")
toHelloWork "56" = Just ("05", "綾部")
toHelloWork "57" = Just ("05", "福知山")
toHelloWork "58" = Just ("06", "舞鶴")
toHelloWork "59" = Just ("07", "宮津")
toHelloWork "60" = Just ("07", "峰山")
toHelloWork "61" = Just ("04", "木津")
toHelloWork "62" = Just ("08", "宇治")
toHelloWork "63" = Just ("04", "京都田辺")
toHelloWork _ = Nothing

toHelloWorkNumber :: Text -> Text
toHelloWorkNumber s = mempty `fromMaybe` (fst <$> toHelloWork s)
