{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Match.Office where

import           Control.Arrow
import           Control.Parallel.Strategies
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Attoparsec.Text
import qualified Data.Map.Strict             as M
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (map)
import qualified Data.Text                   as Tx
import qualified Data.Text.Lazy.Builder      as BB
import           Match.Config                ( Config (..)
                                             , officeSpecF)
import           Match.SQL                   ( fetchSQL
                                             , fetchSQLSource)
import           Match.Base                  (Office (..)
                                             , killBlanks
                                             , killHyphen
                                             , makeKey
                                             , officeTypeRegularize)
import qualified Text.ParseCSV               as T
import           Util.Strbt                  (strdt)

type MaybeIO a = IO (Either String a)

initializeCSVData :: IO T.CSV
initializeCSVData = do
  spec <- officeSpecF
  fetchSQL officeFile spec officeDB

initializeCSVSource :: Source IO [Text]
initializeCSVSource = do
  spec <- officeSpecF
  fetchSQLSource officeFile spec officeDB

makeOffice :: [Text] -> Office
makeOffice record = case record of
  [_code, _name, _own, _pt, _pre, _ad1, _ad2,
   _tel, _fax, _got, _lost, _mb, _cd, _shibu, _k]
    -> O { owner        = _own
         , officePostal = _pt
         , officeAd     = _pre <> _ad1 <> _ad2
         , officeAd1    = _ad1
         , officeAd2    = _ad2
         , officeTel    = _tel
         , officeFax    = _fax
         , officeGot    = strdt _got
         , officeLost   = if _lost == "" then Nothing else strdt _lost
         , shibu        = _shibu
         , officeType   = _mb
         , rosaiCode    = ""
         , officeCode   = _code
         , officeName   = officeTypeRegularize $ killBlanks _name
         , rosaiNumber  = ""
         , koyouNumber  = _cd }
  _ -> error "must not be happen"

initializeData :: IO [Office]
initializeData = do
  csv <- initializeCSVData
  return $ parMap rseq makeOffice csv

initializeSource :: Source IO Office
initializeSource = initializeCSVSource $= CL.map makeOffice

numberMap :: IO (M.Map Text Office)
numberMap = do
  gendata <- initializeData
  return $
    M.fromList . parMap rseq (\n -> (makeKey 6 $ officeCode n, n)) $ gendata

telMap :: IO (M.Map Text Office)
telMap = do
  genData <- initializeData
  return $
    M.fromList . parMap rseq ((killHyphen . officeTel) &&& id) $ genData

posMap :: IO (M.Map Text Office)
posMap = do
  genData <- initializeData
  return $
    M.fromList . parMap rseq ((killHyphen . officePostal) &&& id) $ genData

nameMap :: IO (M.Map Text Office)
nameMap = do
  genData <- initializeData
  return $
    M.fromList . parMap rseq (officeName &&& id) $ genData

makeMapFunc :: (Office -> (Text, Office)) -> IO (M.Map Text Office)
makeMapFunc f = M.fromList <$> (initializeSource =$ CL.map f $$ CL.consume)

numberCMap, telCMap, posCMap, nameCMap :: IO (M.Map Text Office)
numberCMap = makeMapFunc (\n -> (makeKey 6 $ officeCode n , n))
telCMap    = makeMapFunc ((killHyphen . officeTel) &&& id)
posCMap    = makeMapFunc ((killHyphen . officePostal) &&& id)
nameCMap   = makeMapFunc (officeName &&& id)

numberInfixAddressP :: Office -> Bool
numberInfixAddressP o =
  let nump   = satisfy $ inClass "-ー－1234567890１２３４５６７８９０"
  in let parser = many1 nump >> endOfInput
  in let answer = parser `parseOnly` officeAd2 o
  in case (answer, officeType o) of
       (Right _, "2") -> True
       (Right _, "5") -> True
       (_, _)  -> False

basicInfo :: Office -> [BB.Builder]
basicInfo o = map (BB.fromText . ($ o)) funcList
  where maybeString (Just s) = Tx.pack $ show s
        maybeString Nothing  = mempty
        funcList = [ shibu
                   , officeCode
                   , owner
                   , officePostal
                   , officeName
                   , officeAd1
                   , officeAd2
                   , officeTel
                   , officeFax
                   , maybeString . officeGot
                   , maybeString . officeLost ]
