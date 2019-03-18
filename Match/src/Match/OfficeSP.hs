{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilies  #-}
module Match.OfficeSP where

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((>>>))
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Ord
import           Data.Maybe                 (fromMaybe)
import qualified Data.Conduit.List          as CL
import qualified Data.Text                  as Tx
import           Data.Text                  hiding (foldl', map, count)
import qualified Data.Text.IO               as Tx
import           Data.Extensible
import           Data.Monoid
import           Data.Time                  ( Day (..))
import           Data.List                  (sortBy)
import qualified Data.Map.Strict            as M
import           Match.SQL
import           Util
import           Util.Strbt                 (strdt)
import qualified System.IO                 as I

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

instance Sourceable OfficeSP where
  source = SQLSource { specGetter    = #officeSPSpec
                     , csvPathGetter = #officeSPFile
                     , dbPathGetter  = #officeSPDB
                     , makeFunction  = makeOfficeSP }

kikanBango :: OfficeSP -> Text
kikanBango o =
  Tx.take 5 $ takeEnd 6 $ o ^. #groupid

salaryDay :: Text -> Text
salaryDay "31" = "末"
salaryDay s = s

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

codeCMap :: IO (M.Map Text OfficeSP)
codeCMap = do
  let xl = initializeSource
           $= CL.filter koyoP
           $$ CL.consume
  xl ===>
    Key (^. #code) `MakeSingletonMap` Value id

rosaiNumberCMap :: IO (M.Map Text OfficeSP)
rosaiNumberCMap = do
  let xl = initializeSource
           $= CL.filter koyoP
           $$ CL.consume
  xl ===>
    Key rosaiNumberKey `MakeSingletonMap` Value id

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
          , kikanBango o
          , o ^. #otype
          , o ^. #id
          , koyouNumberHeader (o ^. #koyouNumber)
          , koyouNumberRest (o ^. #koyouNumber)
          , o ^. #name
          , o ^. #payEnd
          , o ^. #payM <> o ^. #payDay
          ]

test = do
  I.hSetEncoding I.stdout I.utf8

  source <- (initializeSource :: Source IO OfficeSP)
            $$ CL.consume

  CL.sourceList (comparing KikanBango `sortBy` source)
   $= CL.filter koyoP
   $$ CL.mapM_ (Tx.putStrLn . outputForNendo)
