module Match.KumiaiOffice where

import           Control.Lens
import           Data.Extensible
import           Data.Text              hiding (map)
import qualified Data.Text.Lazy.Builder as BB
import           Match.Base             (killHyphen
                                        , officeTypeRegularize)

type MaybeIO a = IO (Either String a)

type KumiaiOffice = Record
  '[ "idNumber"   >: Text
   , "code"       >: Text
   , "shibu"      >: Text
   , "name"       >: Text
   , "kana"       >: Text
   , "bunkaiCode" >: Text
   , "bunkai"     >: Text
   , "ownerName"  >: Text
   , "ownerKana"  >: Text
   , "postal"     >: Text
   , "tel"        >: Text
   ]

makeKumiaiOffice :: [Text] -> KumiaiOffice
makeKumiaiOffice s = case s of
  [_koid, _koco, _kosb, _koname, _kokana, _kobcode
    , _kobunkai, _koOname, _koOkana, _kopost, _kotel] ->
    #idNumber      @= _koid
    <: #code       @= _koco
    <: #shibu      @= _kosb
    <: #name       @= _koname
    <: #kana       @= _kokana
    <: #bunkaiCode @= _kobcode
    <: #bunkai     @= _kobunkai
    <: #ownerName  @= officeTypeRegularize _koOname
    <: #ownerKana  @= _koOkana
    <: #postal     @= killHyphen _kopost
    <: #tel        @= killHyphen _kotel
    <: nil
  _ -> error "must not be happen."

stringList :: KumiaiOffice -> [BB.Builder]
stringList k = map (BB.fromText . (k ^.)) funcList
  where
    funcList = [ #idNumber
               , #code
               , #shibu
               , #name
               , #kana
               , #bunkaiCode
               , #bunkai
               , #ownerName
               , #ownerKana
               , #postal
               , #tel]

