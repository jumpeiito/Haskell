-- -*- coding:utf-8 -*-
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
module Match.Hiho where

import           Control.Arrow              ((&&&), (>>>))
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Attoparsec.Text       hiding (number)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Either                (isRight)
import           Data.Extensible
import qualified Data.List                  as DL
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (isNothing, isJust
                                            , catMaybes, fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import qualified Data.Set                   as S
import qualified Data.Text                  as Tx
import           Data.Text                  hiding (foldl', map)
import qualified Data.Text.IO               as Tx
import           Data.Time                  ( Day (..)
                                            , fromGregorian)
import           Data.Time.Calendar         (diffDays, toGregorian)
import           Match.Base                 (killBlanks, katakanaP)
import           Match.SQL
import qualified System.IO                  as I
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Heterocephalus        (compileTextFile)
import           Text.Read                  (readMaybe)
import           Util
import           Util.Strbt                 (strdt, howOld)

type HihoR = Record
  '[ "name"        >: Text
   , "code"        >: Text
   , "kana"        >: Text
   , "birth"       >: Maybe Day
   , "rawBirth"    >: Text
   , "postal"      >: Text
   , "address"     >: Text
   , "address1"    >: Text
   , "address2"    >: Text
   , "telnum"      >: Text
   , "number"      >: Text
   , "got"         >: Maybe Day
   , "lost"        >: Maybe Day
   , "alien"       >: Maybe Text
   , "shibu"       >: Maybe Text
   , "officeCode"  >: Text
   , "officeName"  >: Text
   , "koyouNumber" >: Text
   , "officeGot"   >: Maybe Day
   , "officeLost"  >: Maybe Day
   , "payAmount"   >: Maybe Int
   , "workLong"    >: Maybe (Int, Int)
   , "payStyle"    >: Text
   , "country"     >: Maybe Text
   , "alienReq"    >: Maybe Text
   ]

type MaybeIO a = IO (Either String a)

newtype HihoX = IdNumber { runHiho :: HihoR }
newtype OldHiho = Birthday { runOH :: HihoR }

instance Eq HihoX where
  x == y = hihoXKey x == hihoXKey y

instance Eq OldHiho where
  x == y =
    let birth = runOH >>> (^. #birth)
    in birth x == birth y

instance Ord HihoX where
  x `compare` y = hihoXKey x `compare` hihoXKey y

instance Ord OldHiho where
  x `compare` y =
    let birth = runOH >>> (^. #birth)
    in birth x `compare` birth y

hihoXKey :: HihoX -> Text
hihoXKey hx =
  let h = runHiho hx
  in h ^. #officeCode <> h ^. #code

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
  [_code, _hcode, _officename, _name, _kana, _birth, _postal
    , _ad1, _ad2, _telnum, _num, _g, _l, _rnum, _knum
    , _alien, _payA, _workHour, _workMinute, _payS
    , _alienCountry, _alienRequirement, _kn
    ]
    -> #name           @= _name
       <: #code        @= _hcode
       <: #kana        @= _kana
       <: #birth       @= strdt _birth
       <: #rawBirth    @= _birth
       <: #postal      @= _postal
       <: #address     @= _ad1 <> _ad2
       <: #address1    @= _ad1
       <: #address2    @= _ad2
       <: #telnum      @= _telnum
       <: #number      @= _num
       <: #got         @= strdt _g
       <: #lost        @= (if _l == "" then Nothing else strdt _l)
       <: #alien       @= stringMaybe _alien
       <: #shibu       @= (Tx.take 5 (Tx.drop 5 _knum) `M.lookup` shibuMap)
       <: #officeCode  @= _code
       <: #officeName  @= _officename
       <: #koyouNumber @= _kn
       <: #officeGot   @= Nothing
       <: #officeLost  @= Nothing
       <: #payAmount   @= (readMaybe $ unpack _payA)
       <: #workLong    @= Nothing
       <: #payStyle    @= payStyleToString _payS
       <: #country     @= stringMaybe _alienCountry
       <: #alienReq    @= stringMaybe _alienRequirement
       <: nil
  _ -> error "must not be happen"

hihoAliveP :: HihoR -> Bool
hihoAliveP = isNothing . (^. #lost)

--------------------------------------------------
--- 年度更新用
--------------------------------------------------
hihoThisNendoP :: Integer -> HihoR -> Bool
hihoThisNendoP y h =
  case h ^. #lost of
    Nothing -> True
    Just ld -> fromGregorian y 4 1 <= ld

outputForNendo :: HihoR -> Text
outputForNendo h =
  toCSV [ oldYear (h ^. #birth)
        , h ^. #name
        , fromDay (h ^. #birth)
        , fromDay (h ^. #got)
        , fromDay (h ^. #lost)
          -- , h ^. #officeCode
        ]
  where
    fromDay d = mempty `fromMaybe` (japaneseStyleDate <$> d)
    oldYear Nothing = mempty
    oldYear (Just d) =
      if (fromGregorian 1954 4 2) > d
      then "高"
      else mempty

japaneseStyleDate :: Day -> Text
japaneseStyleDate d = Tx.pack ds
  where
    ds = case toGregorian d of
           (y, m, d)
             | y < 1925 -> "T" <> show (y - 1886) <> "." <> show m <> "." <> show d
             | y < 1989 -> "S" <> show (y - 1925) <> "." <> show m <> "." <> show d
             | otherwise -> "H" <> show (y - 1988) <> "." <> show m <> "." <> show d
           _ -> ""

hihoOfficeAliveP :: HihoR -> Bool
hihoOfficeAliveP = isNothing . (^. #officeLost)

-- (&&) <$> even <*> (>= 4)
-- -> do e <- even; f <- (>= 4); return (e && f)
-- -> even >>= \e -> (>= 4) >>= \f -> return (e && f)
hihoNameUnfinishedP :: HihoR -> Bool
hihoNameUnfinishedP = (&&) <$> hihoNameKatakanaP
                           <*> hihoAliveP

hihoNameKatakanaP :: HihoR -> Bool
hihoNameKatakanaP h =
  -- let parser = (many1 $ satisfy inKatakana)
  -- in let answer = parser `parseOnly` (h ^. #name)
  -- in (isRight answer && (isNothing $ h ^. #alien))
  katakanaP (h ^. #name) && (isNothing $ h ^. #alien)

hihoAddressBlankP :: HihoR -> Bool
hihoAddressBlankP =
  (&&) <$> ((^. #lost) >>> isNothing)
       <*> ((^. #address) >>> (== ""))

instance Sourceable HihoR where
  source = SQLSource { specGetter    = #hihoSpec
                     , csvPathGetter = #hihoFile
                     , dbPathGetter  = #hihoDB
                     , makeFunction  = makeHiho }

birthMap :: IO (M.Map (Maybe Day) [HihoR])
birthMap = do
  initializeList ===>
    Key (^. #birth) `MakeListMap` Value id

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

officeCodeCMap :: IO (M.Map Text [HihoR])
officeCodeCMap = do
  let insert mp el =
        M.insertWith (++) (el ^. #officeCode) [el] mp
  initializeSource
    $= CL.filter (hihoThisNendoP 2018)
    $$ CL.fold insert M.empty

koyouNumberCMap :: IO (M.Map Text [HihoR])
koyouNumberCMap = do
  let insert mp el =
        M.insertWith (++) (el ^. #koyouNumber) [el] mp
  initializeSource
    $= CL.filter (hihoThisNendoP 2018)
    $$ CL.fold insert M.empty

howLongD :: HihoR -> Maybe Integer
howLongD h =
  let thisDay = fromGregorian 2019 2 5
  in let days = flip diffDays thisDay <$> h ^. #got
  in abs <$> days

howLongY :: HihoR -> Maybe Integer
howLongY h =
  let days = 365.0 :: Double
  in (ceiling . (/ days) . realToFrac) <$> howLongD h

payStyleToString :: Tx.Text -> Tx.Text
payStyleToString s =
  case s of
    "1" -> "月給"
    "2" -> "週給"
    "3" -> "日給"
    "4" -> "時給"
    _   -> "不明"

hihoOutput :: HihoR -> Tx.Text
hihoOutput h =
  let s = maybeS $ h ^. #shibu
  in let n = h ^. #name
  in let thisDay = fromGregorian 2019 2 6
  in let o  = h ^. #officeName
  in let c  = maybeS $ h ^. #country
  in let y  = maybeS $ xShow <$> howLongY h
  in let p  = h ^. #payStyle
  in let a  = maybeS (xShow <$> h ^. #payAmount)
  in let ho = flip howOld thisDay <$> h ^. #birth
  in let hh = maybeS (xShow <$> ho)
  in let g  = maybeS (xShow <$> (h ^. #got))
  in let l  = maybeS (xShow <$> (h ^. #lost))
  in toCSV [s, n, o, hh, c, y, p, a, g, l]

type Calcurate a = ReaderT Bool IO a
type HihoRList a = [(Maybe a, [HihoR])]

alienIO :: Bool -> IO ()
alienIO b = do
  source <- alienCalcurateSource `runReaderT` b
  runConduit
    $ source .| CL.mapM_ (Tx.putStrLn . hihoOutput)

alienAllOutput :: IO ()
alienAllOutput =
  runConduit
    $ alienSource
    .| CL.mapM_ (Tx.putStrLn . hihoOutput)

payStyleOutputVerbose :: Bool -> IO ()
payStyleOutputVerbose b = do
  gen <- groupByPayStyle `runReaderT` b
  let len = map (\(p, l) -> (p, calcurateState l)) gen
  (liftIO . I.putStrLn) $ renderMarkup
    $(compileTextFile "src/payStyleOutput.txt")

calcurateState :: [HihoR] -> (Int, Int, Double, Double)
calcurateState hs =
  (Prelude.length hs, payMax hs, payAverage hs, payMedian hs)

groupOutput :: Calcurate (HihoRList Text) -> Calcurate ()
groupOutput f = do
  gen <- f
  let len = map (\(s, l) -> (s, Prelude.length l)) gen
  let l = DL.sortBy (comparing snd) len
  (liftIO . I.putStrLn) $ renderMarkup
    $(compileTextFile "src/groupOutput2.txt")

shibuOutput, countryOutput, payStyleOutput :: Bool -> IO ()
shibuOutput    b = groupOutput groupByShibu `runReaderT` b
countryOutput  b = groupOutput groupByCountry `runReaderT` b
payStyleOutput b = groupOutput groupByPayStyle `runReaderT` b

groupByFR :: Ord k => (HihoR -> Maybe k) -> Calcurate (HihoRList k)
groupByFR f = do
  l <- alienCalcurateList
  return . M.toList $
    Key f `MakeListMap` Value id `mapGenerate` l

groupByCountry, groupByPayStyle, groupByShibu :: Calcurate (HihoRList Text)
groupByHowLong :: Calcurate (HihoRList Integer)
groupByCountry  = groupByFR (^. #country)
groupByHowLong  = groupByFR howLongY
groupByPayStyle = groupByFR (Just . (^. #payStyle))
groupByShibu    = groupByFR (^. #shibu)

payMax :: [HihoR] -> Int
payMax h =
  case catMaybes $ map (^. #payAmount) h of
    [] -> 0
    p  -> Prelude.maximum p

payAverage :: [HihoR] -> Double
payAverage h =
  case catMaybes $ map (^. #payAmount) h of
    [] -> 0
    p  -> do
      let r = realToFrac (sum p) / realToFrac (Prelude.length p)
      let mp = 100.0 :: Double
      (fromIntegral $ ceiling $ r * mp) / mp

payMedian :: [HihoR] -> Double
payMedian h =
  case catMaybes $ map (^. #payAmount) h of
    [] -> 0
    p  -> median (map fromIntegral p)

alienCalcurateSource :: Calcurate (Source IO HihoR)
alienCalcurateSource = do
  doFilter <- ask
  let f | doFilter  = hihoAliveP
        | otherwise = const True
  return $ alienSource .| CL.filter f

alienCalcurateList :: Calcurate [HihoR]
alienCalcurateList = do
  doFilter <- ask
  let f | doFilter  = hihoAliveP
        | otherwise = const True
  lift . runConduit
    $ alienSource .| CL.filter f .| CL.consume

alienSource :: Source IO HihoR
alienSource =
  alienSource1 <> alienSource2

alienSource1 :: Source IO HihoR
alienSource1 =
  initializeSource
  .| CL.filter hihoNameKatakanaP
  .| CL.filter (isNothing . (^. #alien))
  .| CL.filter ((`S.member` alienSet) . (^. #number))

alienSource2 :: Source IO HihoR
alienSource2 =
  initializeSource
  .| CL.filter (isJust . (^. #alien))

alienSet :: S.Set Tx.Text
alienSet = S.fromList [
 "51007036896"
 , "50918392341"
 , "50918392287"
 , "50975034295"
 , "50970904495"
 , "50993182730"
 , "50993116676"
 , "50997542326"
 , "50997542501"
 , "51005577699"
 , "50998104069"
 , "50998102773"
 , "50870709787"
 , "50994361426"
 , "50994361802"
 , "50971017593"
 , "50971017725"
 , "50971017740"
 , "51013463748"
 , "51013463160"
 , "51013449274"
 , "50967048453"
 , "50916248383"
 , "50916248301"
 , "50916248262"
 , "50972187047"
 , "50972186950"
 , "51004722258"
 , "50978845092"
 , "50936771639"
 , "50879777400"
 , "50879777519"
 , "50907375638"
 , "50907375517"
 , "50974797079"
 , "50974797306"
 , "50980031427"
 , "51009482102"
 , "50968688507"
 , "50968688563"
 , "51006596823"
 , "51006960466"
 , "51006960533"
 , "51008868016"
 , "51008868085"
 , "51001308240"
 , "51001307564"
 , "50972512549"
 , "50972512616"
 , "51002487292"
 , "51002485832"
 , "51009008969"
 , "51009007571"
 , "50991506732"
 , "50991502258"
 , "50943156303"
 , "50825016836"
 , "50941933348"
 , "51006219348"
 , "51006218739"
 , "50960252279"
 , "50960249527"
 , "50983108639"
 , "50972202005"
 , "51005233209"
 , "51005233317"
 , "50937714011"
 , "50885804252"
 , "51005160607"
 , "50817483985"
 , "50871721726"
 , "50936463052"
 , "50973220347"
 , "50973217314"
 , "50998302383"
 , "51006024834"
 , "50924640851"
 , "50924640916"
 , "50924641002"
 , "50999241452"
 , "50999242502"
 , "50999242018"
 , "51002370634"
 , "51002370647"
 , "50966641685"
 , "50966643171"
 , "51005930515"
 , "51005930139"
 , "50986461385"
 , "50986462651"
 , "50986462809"
 , "51007117292"
 , "51005714290"
 , "50972054805"
 , "50972055239"
 , "50972055120"
 , "50934840648"
 , "50934840903"
 , "50934840501"
 , "50993408741"
 , "51025325815"
 , "51025325117"
 , "51030774646"
 , "51034212191"
 , "51034212604"
  ]

median :: [Double] -> Double
median [] = 0
median ns =
    let l = Prelude.length ns
        n = div l 2
        ms = DL.sort ns
    in if mod l 2 == 0 then
        ((ms !! (n - 1)) + (ms !! n)) / 2
    else
        (ms !! n)

-- 不明67人, ネパール1人, ベトナム46人, 中国8人
-- 1年未満67人 (16.84万), 2年未満37人(19.5万), 3年未満18人(不明)
-- 不明60人(不明), 月給31人(最高25万, 平均17.85万, 中央値16.5万), 日給8人(最高23万, 18.0万, 中央値17.5万), 時給23人(最高20万, 15.57万, 中央値15.0万)
