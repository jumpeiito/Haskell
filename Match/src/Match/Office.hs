{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilies  #-}
module Match.Office where

import           Control.Arrow
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.Attoparsec.Text
import           Data.Extensible
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              hiding (map, count)
import qualified Data.Text              as Tx
import qualified Data.Text.Lazy.Builder as BB
import           Match.SQL
import           Match.Base             (Office
                                        , killBlanks
                                        , killHyphen
                                        , makeKey
                                        , toShibu
                                        , officeTypeRegularize)
import           Util
import           Util.Strbt             (strdt)

type MaybeIO a = IO (Either String a)

makeOffice :: [Text] -> Office
makeOffice line' = case line' of
  [_code, _name, _own, _pt, _pre, _ad1, _ad2,
   _tel, _fax, _got, _lost, _mb, _cd, _shibu, _k,
   _sal_end, _sal_payM, _sal_pay, _hw ] ->
    #owner          @= _own
    <: #postal      @= _pt
    <: #address     @= _pre <> _ad1 <> _ad2
    <: #address1    @= _ad1
    <: #address2    @= _ad2
    <: #tel         @= _tel
    <: #fax         @= _fax
    <: #got         @= strdt _got
    <: #lost        @= (if _lost == "" then Nothing else strdt _lost)
    <: #code        @= _code
    -- <: #name        @= (officeTypeRegularize $ killBlanks _name)
    <: #name        @= (officeTypeRegularize _name)
    <: #shibu       @= _shibu
    <: #otype       @= _mb
    <: #rosaiCode   @= _cd
    <: #rosaiNumber @= ""
    <: #koyouNumber @= _k
    <: #salaryEnd   @= (salaryDay _sal_end)
    <: #salaryPayM  @= (salaryMonth _sal_payM)
    <: #salaryPay   @= (salaryDay _sal_pay)
    <: #hellowork   @= _hw
    <: nil
  _ -> error $ Tx.unpack $ "must not be happen : " <> Tx.intercalate "," line'

koyoP :: Office -> Bool
koyoP o | (o ^. #otype) == "0" = True
        | (o ^. #otype) == "2" = True
        | otherwise = False

outOfP :: Office -> Bool
outOfP o | Tx.take 2 (o ^. #koyouNumber) == "26" = False
         | otherwise = True

salaryDay :: Text -> Text
salaryDay "31" = "末"
salaryDay s = s

salaryMonth :: Text -> Text
salaryMonth "1" = "当月"
salaryMonth "2" = "翌月"
salaryMonth _   = ""

instance Sourceable Office where
  source = SQLSource { specGetter    = #officeSpec
                     , csvPathGetter = #officeFile
                     , dbPathGetter  = #officeDB
                     , makeFunction  = makeOffice }

newtype OfficeX = KikanBango { runOffice :: Office }

instance Eq OfficeX where
  x == y = makeOfficeXKey x == makeOfficeXKey y

instance Ord OfficeX where
  x `compare` y =
    -- makeOfficeXKey x `compare` makeOfficeXKey y
    let toKikan o =
          mempty `fromMaybe` toKikanBango (o ^. #shibu)
    in let edaban o =
             (o ^. #otype) <> (o ^. #rosaiCode)
    in (toKikan (runOffice x) `compare` toKikan (runOffice y)) <>
       (edaban (runOffice x) `compare` edaban (runOffice y))

rosaiNumberKey :: Office -> Text
rosaiNumberKey o = kikan <> ot <> edaban
  where
    kikan =
      mempty `fromMaybe` toKikanBango (o ^. #shibu)
    ot = o ^. #otype
    edaban = o ^. #rosaiCode

makeOfficeXKey :: OfficeX -> Text
makeOfficeXKey ox =
  let o = runOffice ox in
    mempty `fromMaybe` toKikanBango (o ^. #shibu)
    <> o ^. #rosaiCode

makeMapFunc :: (Office -> (Text, Office)) -> IO (M.Map Text Office)
makeMapFunc f = M.fromList <$> (initializeSource =$ CL.map f $$ CL.consume)

numberMap, telMap, posMap, nameMap :: IO (M.Map Text Office)
numberMap = makeMapFunc (\n -> (makeKey 6 $ n ^. #code, n))
telMap    = makeMapFunc ((killHyphen . (^. #tel)) &&& id)
posMap    = makeMapFunc ((killHyphen . (^. #postal)) &&& id)
nameMap   = makeMapFunc ((^. #name) &&& id)

numberInfixAddressP :: Office -> Bool
numberInfixAddressP o =
  let nump   = satisfy $ inClass "-ー－1234567890１２３４５６７８９０"
  in let parser = many1 nump >> endOfInput
  in let answer = parser `parseOnly` (o ^. #address2)
  in case (answer, o ^. #otype) of
       (Right _, "2") -> True
       (Right _, "5") -> True
       (_, _)  -> False

basicInfo :: Office -> [BB.Builder]
basicInfo o = map (BB.fromText . (o ^.)) funcList
  where
    funcList = [
      #shibu
      , #otype
      , #rosaiCode
      , #code
      , #koyouNumber
      -- , #owner
      -- , #postal
      , #name
        -- , #address1
        -- , #address2
        -- , #tel
        -- , #fax
      ]

--------------------------------------------------
--- 年度更新用
--------------------------------------------------
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

toKikanBango :: Text -> Maybe Text
toKikanBango "10" = Just "93075"
toKikanBango "11" = Just "93076"
toKikanBango "12" = Just "93078"
toKikanBango "13" = Just "93253"
toKikanBango "14" = Just "93254"
toKikanBango "15" = Just "93077"
toKikanBango "16" = Just "93252"
toKikanBango "17" = Just "93276"
toKikanBango "18" = Just "93079"
toKikanBango "19" = Just "93080"
toKikanBango "20" = Just "93320"
toKikanBango "21" = Just "93352"
toKikanBango "50" = Just "93255"
toKikanBango "51" = Just "93809"
toKikanBango "53" = Just "93140"
toKikanBango "54" = Just "93081"
toKikanBango "56" = Just "93507"
toKikanBango "57" = Just "93506"
toKikanBango "58" = Just "93612"
toKikanBango "59" = Just "93719"
toKikanBango "60" = Just "93718"
toKikanBango "61" = Just "93416"
toKikanBango "62" = Just "93824"
toKikanBango "63" = Just "93419"
toKikanBango _ = Nothing

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

helloworkForNendo :: Office -> Text
helloworkForNendo o =
  if (outOfP o) then o ^. #hellowork else ""


outputForNendo :: Office -> Text
outputForNendo o = toCSV txs
  where
    s'  = case o ^. #shibu of
            "" -> mempty
            o' -> toShibu $ read $ Tx.unpack o'
    txs = [ o ^. #shibu
          , mempty `fromMaybe` (Tx.pack <$> s')
          , mempty `fromMaybe` (toKikanBango (o ^. #shibu))
          , o ^. #otype
          , Tx.takeEnd 3 (o ^. #rosaiCode)
          , koyouNumberHeader (o ^. #koyouNumber)
          , koyouNumberRest (o ^. #koyouNumber)
          , o ^. #name
          , o ^. #salaryEnd
          , o ^. #salaryPayM <> o ^. #salaryPay
          ]
