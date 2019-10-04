{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
module Match.Map where

import           Control.Arrow      ((&&&), (>>>))
import           Control.Lens
import           Control.Monad.Reader
import           Data.Conduit
import qualified Data.Conduit.List  as CL
import qualified Data.Map.Strict    as M
import           Data.Maybe
import qualified Data.Text          as Tx
import           Data.Text          hiding (foldl', map)
import           Data.Time          (Day (..))
import qualified Match.Base         as B
import           Match.SQL          ( initializeSource
                                    , initializeList)
import qualified Match.Hiho         as H
import qualified Match.Hitori       as HT
import qualified Match.OfficeSP     as OSP
import qualified Match.Kumiai       as K
import qualified Match.KumiaiOffice as KO
import           Util

-- hiho
hihoBirthMap :: IO (M.Map (Maybe Day) [H.HihoR])
hihoBirthMap = do
  initializeList ===>
    Key (^. #birth) `MakeListMap` Value id

hihoKanaBirthMap :: IO (M.Map (Text, Maybe Day) [H.HihoR])
hihoKanaBirthMap = do
  initializeList ===>
    Key ((^. #kana) &&& (^. #birth)) `MakeListMap` Value id

hihoNumberMap :: IO (M.Map Text H.HihoR)
hihoNumberMap = do
  initializeList ===>
    Key (^. #number) `MakeSingletonMap` Value id

hihoKanaBirthCMap :: IO (M.Map (Text, Maybe Day) [H.HihoR])
hihoKanaBirthCMap = do
  let insert mp el =
        M.insertWith (++)
                     (B.killBlanks (el ^. #kana), el ^. #birth)
                     [el] mp
  initializeSource $$ CL.fold insert M.empty

hihoKanaShibuBirthCMap :: IO (M.Map (Text, Text, Maybe Day) [H.HihoR])
hihoKanaShibuBirthCMap = do
  let triple el =
        (B.killBlanks (el ^. #kana),
         "" `fromMaybe` (el ^. #shibu),
         el ^. #birth)
  let insert mp el =
        M.insertWith (++) (triple el) [el] mp
  initializeSource $$ CL.fold insert M.empty

hihoNumberCMap :: IO (M.Map Text H.HihoR)
hihoNumberCMap = do
  gen <- initializeSource
         =$ CL.map ((^. #number) &&& id)
         $$ CL.consume
  return $ M.fromList gen

hihoOfficeCodeCMap :: IO (M.Map Text [H.HihoR])
hihoOfficeCodeCMap = do
  let insert mp el =
        M.insertWith (++)
                     (el ^. #officeCode)
                     [el] mp
  initializeSource
    $= CL.filter (H.hihoThisNendoP 2018)
    $$ CL.fold insert M.empty

hihoKoyouNumberCMap :: IO (M.Map Text [H.HihoR])
hihoKoyouNumberCMap = do
  let insert mp el =
        M.insertWith (++) (el ^. #koyouNumber) [el] mp
  initializeSource
    $= CL.filter (H.hihoThisNendoP 2018)
    $$ CL.fold insert M.empty

hihoKoyouNumberRMap :: ReaderT Integer IO (M.Map Text [H.HihoR])
hihoKoyouNumberRMap = do
  thisYear <- ask
  let insert mp el =
        M.insertWith (++) (el ^. #koyouNumber) [el] mp
  liftIO . runConduit $
    initializeSource
    .| CL.filter (H.hihoThisNendoP thisYear)
    .| CL.fold insert M.empty

-- office
makeOfficeMap ::
  (B.Office -> (Text, B.Office)) -> IO (M.Map Text B.Office)
makeOfficeMap f =
  M.fromList <$> (initializeSource =$ CL.map f $$ CL.consume)

officeNumberMap, officeTelMap, officePosMap, officeNameMap
  :: IO (M.Map Text B.Office)
officeNumberMap = makeOfficeMap (\n -> (B.makeKey 6 $ n ^. #code, n))
officeTelMap    = makeOfficeMap ((B.killHyphen . (^. #tel)) &&& id)
officePosMap    = makeOfficeMap ((B.killHyphen . (^. #postal)) &&& id)
officeNameMap   = makeOfficeMap ((^. #name) &&& id)

-- kumiai
kumiaiMakeKey :: Text -> Text
kumiaiMakeKey = Tx.take 6 . B.makeKey 7

kumiaiNumberMap :: IO (M.Map Text K.Kumiai)
kumiaiNumberMap = do
  let k = kumiaiMakeKey . (^. #number)
  initializeList ===> Key k `MakeSingletonMap` Value id

kumiaiBirthdayMap :: IO (M.Map (Maybe Day) [K.Kumiai])
kumiaiBirthdayMap = do
  initializeList ===>
    Key (^. #birth) `MakeListMap` Value id

kumiaiBirthdayNameMap :: IO (M.Map (Text, Maybe Day) [K.Kumiai])
kumiaiBirthdayNameMap = do
  let toKey el = (B.killBlanks $ el ^. #kana, el ^. #birth)
  initializeList ===>
    Key toKey `MakeListMap` Value id

kumiaiNumberCMap :: IO (M.Map Text K.Kumiai)
kumiaiNumberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((kumiaiMakeKey . (^. #number)) &&& id)
              $$ CL.consume)

kumiaiBirthdayCMap :: IO (M.Map (Maybe Day) [K.Kumiai])
kumiaiBirthdayCMap = do
  let insert mp el =
        M.insertWith (++) (el ^. #birth) [el] mp
  initializeSource $$ CL.fold insert M.empty

kumiaiBirthdayNameCMap :: IO (M.Map (Text, Maybe Day) [K.Kumiai])
kumiaiBirthdayNameCMap = do
  let insert mp el =
        let b = el ^. #birth
        in let k = B.killBlanks $ el ^. #kana
        in M.insertWith (++) (k, b) [el] mp
  initializeSource $$ CL.fold insert M.empty

kumiaiOfficeCodeMap :: IO (M.Map Text [K.Kumiai])
kumiaiOfficeCodeMap = do
  ls <- runConduit
        $ initializeSource
        .| CL.consume
  let rize = Tx.justifyRight 7 '0'
  return $
    ls ==> Key (rize . (^. #officeCode)) `MakeListMap` Value id

-- kumiaiOffice
makeKeySimplize :: Text -> Text
makeKeySimplize = Tx.take 6 . B.makeKey 6 . Tx.drop 3

koNumberMap :: IO (M.Map Text KO.KumiaiOffice)
koNumberMap = do
  initializeList ===>
    Key (makeKeySimplize . (^. #idNumber))
      `MakeSingletonMap` Value id

koNameMap :: IO (M.Map Text KO.KumiaiOffice)
koNameMap =
  initializeList ===>
    Key (^. #name) `MakeSingletonMap` Value id

koNumberCMap :: IO (M.Map Text KO.KumiaiOffice)
koNumberCMap = M.fromList <$>
             (initializeSource
              =$ CL.map ((makeKeySimplize . (^. #idNumber)) &&& id)
              $$ CL.consume)

koNameCMap :: IO (M.Map Text KO.KumiaiOffice)
koNameCMap = M.fromList <$>
           (initializeSource
             =$ CL.map ((^. #name) &&& id)
             $$ CL.consume)

-- officeSP
ospCodeCMap :: IO (M.Map Text OSP.OfficeSP)
ospCodeCMap = do
  let xl = initializeSource
           $= CL.filter OSP.koyoP
           $$ CL.consume
  xl ===>
    Key (^. #code) `MakeSingletonMap` Value id

ospRosaiNumberCMap :: IO (M.Map Text OSP.OfficeSP)
ospRosaiNumberCMap = do
  let xl = initializeSource
           $= CL.filter OSP.koyoP
           $$ CL.consume
  xl ===>
    Key OSP.rosaiNumberKey `MakeSingletonMap` Value id

-- hitori
hitoriRosaiCodeMap :: IO (M.Map Text Text)
hitoriRosaiCodeMap = do
  let personsCSV =
        (^. #persons) >>> map HT.personString >>> toCSV
  let rosaiCode =
        HT.rosaiCode >>> Tx.takeEnd 10
        -- HT.rosaiCode
  M.fromList <$>
    (initializeSource
     $= CL.map (rosaiCode &&& personsCSV)
     $$ CL.consume)

hitoriRosaiCodeMap2 :: IO (M.Map Text HT.Hitori)
hitoriRosaiCodeMap2 = do
  let rosaiCode =
        HT.rosaiCode >>> Tx.takeEnd 10
        -- HT.rosaiCode
  M.fromList <$>
    (initializeSource
     $= CL.map (rosaiCode &&& id)
     $$ CL.consume)

hitori2NumberMap :: IO (M.Map Text HT.Hitori)
hitori2NumberMap = do
  -- let rosaiCode =
  --       HT.rosaiCode >>> Tx.takeEnd 10
        -- HT.rosaiCode
  M.fromList <$>
    (initializeSource
     $= CL.map HT.runHO
     $= CL.map ((^. #code) &&& id)
     $$ CL.consume)
