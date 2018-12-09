{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Match.Hiho where

import           Control.Arrow               ((&&&))
import           Control.Parallel.Strategies
import           Data.Attoparsec.Text        hiding (number)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.List                   (foldl')
import           Data.Either                 (isRight)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (foldl', map)
import qualified Data.Text                   as Tx
import           Data.Time                   (Day (..))
import           GHC.Generics
import           Match.Config                (Config (..), hihoSpecF)
import           Match.SQL                   ( fetchSQL
                                             , fetchSQLSource)
import           Match.Base                  (BaseInfo (..)
                                             , Office (..)
                                             , killBlanks, regularize)
import           Util.Strbt                  (strdt)

data Hiho = H {
  info        :: ! BaseInfo
  , postal    :: ! Text
  , address   :: ! Text
  , address1  :: ! Text
  , address2  :: ! Text
  , telnum    :: ! Text
  , number    :: ! Text
  , got       :: Maybe Day
  , lost      :: Maybe Day
  , alien     :: Maybe Text
  , hihoShibu :: Maybe Text
  , office    :: ! Office } deriving (Show, Generic, Eq)

type MaybeIO a = IO (Either String a)

shibuMap :: M.Map Text Text
shibuMap = M.fromList
  [ ("93075", "10")
  , ("93076", "11")
  , ("93078", "12")
  , ("93253", "13")
  , ("93254", "14")
  , ("93077", "15")
  , ("93252", "16")
  , ("93276", "17")
  , ("93079", "18")
  , ("93080", "19")
  , ("93320", "20")
  , ("93352", "21")
  , ("93255", "50")
  , ("93809", "51")
  , ("93824", "62")
  , ("93419", "63")
  , ("93416", "61")
  , ("93140", "53")
  , ("93081", "54")
  , ("93507", "56")
  , ("93506", "57")
  , ("93612", "58")
  , ("93719", "59")
  , ("93718", "60")]

stringMaybe :: Text -> Maybe Text
stringMaybe "" = Nothing
stringMaybe s  = Just s

initializeCSVData :: IO [[Text]]
initializeCSVData = do
  spec <- hihoSpecF
  fetchSQL hihoFile spec hihoDB

initializeCSVSource :: Source IO [Text]
initializeCSVSource = do
  spec <- hihoSpecF
  fetchSQLSource hihoFile spec hihoDB

makeHiho :: [Text] -> Hiho
makeHiho record = case record of
  [_code, _officename, _name, _kana, _birth, _postal,
   _ad1, _ad2, _telnum, _num, _g, _l, _rnum, _knum, _alien]
    -> H { postal    = _postal
         , address   = _ad1 <> _ad2
         , address1  = _ad1
         , address2  = _ad2
         , telnum    = _telnum
         , number    = _num
         , got       = strdt _g
         , lost      = if _l == "" then Nothing else strdt _l
         , alien     = stringMaybe _alien
         , hihoShibu = Tx.take 5 (Tx.drop 5 _knum) `M.lookup` shibuMap
         , info      = B { infoName  = _name
                         , infoKana  = regularize $ killBlanks _kana
                         , infoBirth = strdt _birth
                         , rawBirth  = _birth }
         , office    = O { owner          = ""
                         , officePostal = ""
                         , officeAd     = ""
                         , officeAd1    = ""
                         , officeAd2    = ""
                         , officeTel    = ""
                         , officeFax    = ""
                         , officeGot    = Nothing
                         , officeLost   = Nothing
                         , shibu        = ""
                         , officeType   = ""
                         , rosaiCode    = ""
                         , officeCode   = _code
                         , officeName   = _officename
                         , rosaiNumber  = _rnum
                         , koyouNumber  = _knum }}
  _ -> error "must not be happen"

hihoName :: Hiho -> Text
hihoName = infoName . info

hihoBirthday :: Hiho -> Maybe Day
hihoBirthday = infoBirth . info

hihoKana :: Hiho -> Text
hihoKana = infoKana . info

hihoAliveP :: Hiho -> Bool
hihoAliveP = isNothing . lost

hihoOfficeAliveP :: Hiho -> Bool
hihoOfficeAliveP = isNothing . officeLost . office

hihoOfficeName, hihoOfficeCode :: Hiho -> Text
hihoOfficeName = officeName . office
hihoOfficeCode = officeCode . office

inKatakana :: Char -> Bool
inKatakana = inClass ['ア'..'ン']

hihoNameUnfinishedP :: Hiho -> Bool
hihoNameUnfinishedP h =
  let parser = (many1 $ satisfy inKatakana)
  in let answer = parser `parseOnly` hihoName h
  -- in and [isRight answer, isNothing $ lost h, isNothing $ alien h]
  in (isRight answer && (isNothing $ lost h) && (isNothing $ alien h))

hihoAddressBlankP :: Hiho -> Bool
hihoAddressBlankP h = (isNothing $ lost h) && (address h == "")

initializeData :: IO [Hiho]
initializeData = do
  csv <- initializeCSVData
  return $ parMap rseq makeHiho csv

initializeSource :: Source IO Hiho
initializeSource = initializeCSVSource $= CL.map makeHiho

kanaBirthMap :: IO (M.Map (Text, Maybe Day) [Hiho])
kanaBirthMap = do
  csv <- initializeData
  let insert mp el =
        M.insertWith (++) (hihoKana el, hihoBirthday el) [el] mp
  return $ foldl' insert M.empty csv

numberMap :: IO (M.Map Text Hiho)
numberMap = do
  csv <- initializeData
  return $
    M.fromList (parMap rseq (number &&& id) csv)

kanaBirthCMap :: IO (M.Map (Text, Maybe Day) [Hiho])
kanaBirthCMap = do
  let insert mp el =
        M.insertWith (++) (hihoKana el, hihoBirthday el) [el] mp
  initializeSource $$ CL.fold insert M.empty

numberCMap :: IO (M.Map Text Hiho)
numberCMap = do
  gen <- initializeSource
         =$ CL.map (number &&& id)
         $$ CL.consume
  return $ M.fromList gen
