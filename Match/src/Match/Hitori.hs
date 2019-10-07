{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
module Match.Hitori where

import           Control.Lens
import           Data.Extensible
import qualified Data.List        as DL
import           Data.Maybe       ( catMaybes
                                  , fromMaybe)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as Tx
import           Data.Time        ( Day (..))
import           Text.Heredoc
import           Util
import           Util.Strbt


type Hitori = Record
  '[ "code"      >: Text
   , "name"      >: Text
   , "nameKana"  >: Text
   , "owner"     >: Text
   , "ownerKana" >: Text
   , "shibu"     >: Text
   , "shibuName" >: Text
   , "otype"     >: Text
   , "id"        >: Text
   , "groupid"   >: Text
   , "persons"   >: [HitoriPerson]
   , "personsRow" >: [Text]
   -- , "persons"   >: [Text]
   ]

type HitoriPerson = Record
  '[ "name"     >: Text
   , "amount"   >: Text
   , "got"      >: Maybe Day
   , "lost"     >: Maybe Day
   , "nameKana" >: Text
   ]

newtype HitoriO = HO { runHO :: Hitori }

kokuhoOutput :: Hitori -> Text
kokuhoOutput h =
  let _id = h ^. #id
  in let _groupid = h ^. #groupid
  in [heredoc|一人親方,${_groupid}-${_id}|]

hitoriOLiveP :: Hitori -> Bool
hitoriOLiveP h = case h ^. #persons of
                   [a] -> case (a ^. #got, a ^. #lost) of
                            (Just _, Nothing) -> True
                            _                 -> False
                   _   -> False

rosaiCode :: Hitori -> Text
rosaiCode = (<>) <$> (^. #groupid) <*> (^. #id)

makeHitori :: [Text] -> Hitori
makeHitori txs = case DL.take 10 txs of
  [_c, _n, _nk, _o, _ok, _s, _sn,
   _ot, _id, _gi] ->
    #code @= _c
    <: #name      @= _n
    <: #nameKana  @= _nk
    <: #owner     @= _o
    <: #ownerKana @= _ok
    <: #shibu     @= _s
    <: #shibuName @= _sn
    <: #otype     @= _ot
    <: #id        @= _id
    <: #groupid   @= _gi
    <: #persons   @= (makeHitoriPerson $ DL.drop 10 txs)
    <: #personsRow @= DL.drop 10 txs
    -- <: #persons   @= (DL.drop 10 txs)
    <: nil
  _ -> error "must not be happen"

makeHitoriPerson :: [Text] -> [HitoriPerson]
makeHitoriPerson = catMaybes . map makeHitoriPerson_ . group 5

makeHitoriPerson_ :: [Text] -> Maybe HitoriPerson
makeHitoriPerson_ txs = case txs of
  ["", _, _, _, _] -> Nothing
  [_n, _a, _g, _l, _nk] ->
    Just (#name @= _n
          <: #amount   @= _a
          <: #got      @= (strdt _g)
          <: #lost     @= (strdt _l)
          <: #nameKana @= _nk
          <: nil)
  _ -> Nothing

personString :: HitoriPerson -> Text
personString hp =
  toCSV [ hp ^. #nameKana
        , hp ^. #name
        , hp ^. #amount
        , mempty `fromMaybe` (Tx.pack . show <$> hp ^. #got)
        , mempty `fromMaybe` (Tx.pack . show <$> hp ^. #lost)
        , hp ^. #amount
        ]
