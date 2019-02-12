{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Match.KumiaiOffice where

import           Control.Lens
import           Control.Arrow          ((&&&))
import           Control.Monad.Reader   (runReader)
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.Extensible
import qualified Data.Map.Strict        as M
import           Data.Text              hiding (map)
import qualified Data.Text              as Tx
import qualified Data.Text.Lazy.Builder as BB
import           Match.SQL
import           Match.Base             (killHyphen
                                        , makeKey
                                        , officeTypeRegularize)
import           Util

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

instance Sourceable KumiaiOffice where
  source = SQLSource { specGetter    = #kumiaiOfficeSpec
                     , csvPathGetter = #kumiaiOfficeFile
                     , dbPathGetter  = #kumiaiOfficeDB
                     , makeFunction  = makeKumiaiOffice }

makeKeySimplize :: Text -> Text
makeKeySimplize = Tx.take 6 . makeKey 6 . Tx.drop 3

numberMap :: IO (M.Map Text KumiaiOffice)
numberMap = do
  initializeList ===>
    Key (makeKeySimplize . (^. #idNumber))
      `MakeSingletonMap` Value id

nameMap :: IO (M.Map Text KumiaiOffice)
nameMap =
  initializeList ===>
    Key (^. #name) `MakeSingletonMap` Value id

numberCMap :: IO (M.Map Text KumiaiOffice)
numberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((makeKeySimplize . (^. #idNumber)) &&& id)
              $$ CL.consume)

nameCMap :: IO (M.Map Text KumiaiOffice)
nameCMap = M.fromList <$>
           (initializeSource
             =$ CL.map ((^. #name) &&& id)
             $$ CL.consume)
