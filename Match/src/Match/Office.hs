{-# LANGUAGE FlexibleContexts  #-}
module Match.Office where

import           Control.Arrow
import           Control.Lens
import           Control.Parallel.Strategies (parMap, rseq)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Attoparsec.Text
import           Data.Extensible
import qualified Data.Map.Strict             as M
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (map)
import qualified Data.Text                   as Tx
import qualified Data.Text.Lazy.Builder      as BB
import           Match.Config                ( Conf (..)
                                             , officeSpecF)
import           Match.SQL                   (fetchSQLSource)
import           Match.Base                  (Office (..)
                                             , killBlanks
                                             , killHyphen
                                             , makeKey
                                             , officeTypeRegularize)
import qualified Text.ParseCSV               as T
import           Util.Strbt                  (strdt)

type MaybeIO a = IO (Either String a)

initializeCSVSource :: Source IO [Text]
initializeCSVSource = do
  spec <- officeSpecF
  fetchSQLSource #officeFile spec #officeDB

makeOffice :: [Text] -> Office
makeOffice record = case record of
  [_code, _name, _own, _pt, _pre, _ad1, _ad2,
   _tel, _fax, _got, _lost, _mb, _cd, _shibu, _k] ->
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
    <: #name        @= (officeTypeRegularize $ killBlanks _name)
    <: #shibu       @= _shibu
    <: #otype       @= _mb
    <: #rosaiCode   @= ""
    <: #rosaiNumber @= ""
    <: #koyouNumber @= _cd
    <: nil
  _ -> error $ Tx.unpack $ "must not be happen : " <> Tx.intercalate "," record

initializeSource :: Source IO Office
initializeSource = initializeCSVSource $= CL.map makeOffice

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
  where maybeString (Just s) = Tx.pack $ show s
        maybeString Nothing  = mempty
        funcList = [ #shibu
                   , #code
                   , #owner
                   , #postal
                   , #name
                   , #address1
                   , #address2
                   , #tel
                   , #fax]
