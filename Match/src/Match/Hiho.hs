-- -*- coding:utf-8 -*-
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
module Match.Hiho where

import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad.Reader        (runReader)
import           Data.Attoparsec.Text        hiding (number)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Either                 (isRight)
import           Data.Extensible
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   hiding (foldl', map)
import qualified Data.Text                   as Tx
import           Data.Time                   (Day (..))
import           Match.Base                  (killBlanks)
import           Match.SQL
import           Util
import           Util.Strbt                  (strdt)

type HihoR = Record
  '[ "name"       >: Text
   , "kana"       >: Text
   , "birth"      >: Maybe Day
   , "rawBirth"   >: Text
   , "postal"     >: Text
   , "address"    >: Text
   , "address1"   >: Text
   , "address2"   >: Text
   , "telnum"     >: Text
   , "number"     >: Text
   , "got"        >: Maybe Day
   , "lost"       >: Maybe Day
   , "alien"      >: Maybe Text
   , "shibu"      >: Maybe Text
   , "officeCode" >: Text
   , "officeName" >: Text
   , "officeGot"  >: Maybe Day
   , "officeLost" >: Maybe Day
   ]

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

makeHiho :: [Text] -> HihoR
makeHiho line' = case line' of
  [_code, _officename, _name, _kana, _birth, _postal,
   _ad1, _ad2, _telnum, _num, _g, _l, _rnum, _knum, _alien]
    -> #name          @= _name
       <: #kana       @= _kana
       <: #birth      @= strdt _birth
       <: #rawBirth   @= _birth
       <: #postal     @= _postal
       <: #address    @= _ad1 <> _ad2
       <: #address1   @= _ad1
       <: #address2   @= _ad2
       <: #telnum     @= _telnum
       <: #number     @= _num
       <: #got        @= strdt _g
       <: #lost       @= (if _l == "" then Nothing else strdt _l)
       <: #alien      @= stringMaybe _alien
       <: #shibu      @= (Tx.take 5 (Tx.drop 5 _knum) `M.lookup` shibuMap)
       <: #officeCode @= _code
       <: #officeName @= _officename
       <: #officeGot  @= Nothing
       <: #officeLost @= Nothing
       <: nil
  _ -> error "must not be happen"

hihoAliveP :: HihoR -> Bool
hihoAliveP = isNothing . (^. #lost)

hihoOfficeAliveP :: HihoR -> Bool
hihoOfficeAliveP = isNothing . (^. #officeLost)

inKatakana :: Char -> Bool
inKatakana = inClass ['ア'..'ン']

hihoNameUnfinishedP :: HihoR -> Bool
hihoNameUnfinishedP h =
  let parser = (many1 $ satisfy inKatakana)
  in let answer = parser `parseOnly` (h ^. #name)
  in (isRight answer && (isNothing $ h ^. #lost) && (isNothing $ h ^. #alien))

hihoAddressBlankP :: HihoR -> Bool
hihoAddressBlankP h = (isNothing $ h ^. #lost) && (h ^. #address == "")

instance Sourceable HihoR where
  source = SQLSource { specGetter    = #hihoSpec
                     , csvPathGetter = #hihoFile
                     , dbPathGetter  = #hihoDB
                     , makeFunction  = makeHiho }

kanaBirthMap :: IO (M.Map (Text, Maybe Day) [HihoR])
kanaBirthMap = do
  initializeList ===>
    Key ((^. #kana) &&& (^. #birth)) `MakeListMap` Value id

numberMap :: IO (M.Map Text HihoR)
numberMap = do
  initializeList ===>
    Key (^. #number) `MakeSingletonMap` Value id

kanaBirthCMap :: IO (M.Map (Text, Maybe Day) [HihoR])
kanaBirthCMap = do
  let insert mp el =
        M.insertWith (++) (killBlanks (el ^. #kana), el ^. #birth) [el] mp
  initializeSource $$ CL.fold insert M.empty

numberCMap :: IO (M.Map Text HihoR)
numberCMap = do
  gen <- initializeSource
         =$ CL.map ((^. #number) &&& id)
         $$ CL.consume
  return $ M.fromList gen
