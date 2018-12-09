{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Match.KumiaiOffice where

import           Control.Parallel.Strategies
import           GHC.Generics
import           Control.Arrow               ((&&&))
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import qualified Data.Map.Strict             as M
import           Data.Text                   hiding (map)
import qualified Data.Text                   as Tx
import qualified Data.Text.Lazy.Builder      as BB
import           Match.Config                ( Config (..)
                                             , kumiaiOfficeSpecF)
import           Match.SQL                   ( fetchSQL
                                             , fetchSQLSource)
import           Match.Base                  (killHyphen
                                             , makeKey
                                             , officeTypeRegularize)

type MaybeIO a = IO (Either String a)

data KumiaiOffice = KO { koIDnumber    :: ! Text
                       , koCode        :: ! Text
                       , koShibu       :: ! Text
                       , koOfficeName  :: ! Text
                       , koOfficeKana  :: ! Text
                       , koOwnerBcode  :: ! Text
                       , koOwnerBunkai :: ! Text
                       , koOwnerName   :: ! Text
                       , koOwnerKana   :: ! Text
                       , koPostal      :: ! Text
                       , koTel         :: ! Text }
  deriving (Generic, NFData)

initializeCSVData :: IO [[Text]]
initializeCSVData = do
  spec <- kumiaiOfficeSpecF
  fetchSQL kumiaiOfficeFile spec kumiaiOfficeDB

initializeCSVSource :: Source IO [Text]
initializeCSVSource = do
  spec <- kumiaiOfficeSpecF
  fetchSQLSource kumiaiOfficeFile spec kumiaiOfficeDB

makeKumiaiOffice :: [Text] -> KumiaiOffice
makeKumiaiOffice s = case s of
  [_koid, _koco, _kosb, _koname, _kokana, _kobcode
    , _kobunkai, _koOname, _koOkana, _kopost, _kotel] ->
    KO { koIDnumber    = _koid
       , koCode        = _koco
       , koShibu       = _kosb
       , koOfficeName  = _koname
       , koOfficeKana  = _kokana
       , koOwnerBcode  = _kobcode
       , koOwnerBunkai = _kobunkai
       , koOwnerName   = officeTypeRegularize _koOname
       , koOwnerKana   = _koOkana
       , koPostal      = killHyphen _kopost
       , koTel         = killHyphen _kotel }
  _ -> error "must not be happen."

stringList :: KumiaiOffice -> [BB.Builder]
stringList k = map (BB.fromText . ($ k)) funcList
  where funcList = [ koIDnumber
                   , koCode
                   , koShibu
                   , koOfficeName
                   , koOfficeKana
                   , koOwnerBcode
                   , koOwnerBunkai
                   , koOwnerName
                   , koOwnerKana
                   , koPostal
                   , koTel
                   ]

initializeData :: IO [KumiaiOffice]
initializeData = do
  csv <- initializeCSVData
  return $ parMap rseq makeKumiaiOffice csv

initializeSource :: Source IO KumiaiOffice
initializeSource = do
  initializeCSVSource $= CL.map makeKumiaiOffice

makeKeySimplize :: Text -> Text
makeKeySimplize = Tx.take 6 . makeKey 6 . Tx.drop 3

numberMap :: IO (M.Map Text KumiaiOffice)
numberMap = do
  csv <- initializeData
  return $
    M.fromList $ parMap rseq ((makeKeySimplize . koIDnumber) &&& id) csv

nameMap :: IO (M.Map Text KumiaiOffice)
nameMap = do
  csv <- initializeData
  return $
    M.fromList $ parMap rseq (koOfficeName &&& id) csv

numberCMap :: IO (M.Map Text KumiaiOffice)
numberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((makeKeySimplize . koIDnumber) &&& id)
              $$ CL.consume)

nameCMap :: IO (M.Map Text KumiaiOffice)
nameCMap = M.fromList <$>
           (initializeSource
             =$ CL.map (koOfficeName &&& id)
             $$ CL.consume)
