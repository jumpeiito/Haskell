{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Match.Figure where

import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Match.Base                as B
import qualified Match.Hiho                as H
import qualified Match.Kumiai              as K
import           Data.Ord                  (comparing)
import           Data.Foldable             (forM_)
import           Data.Maybe                (fromMaybe, isNothing, isJust, fromJust)
import           Data.Text                 (Text, pack)
import           Data.Monoid               ((<>))
import qualified Data.Text.Lazy.Builder    as TLB
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.IO              as Txio
import           Data.Conduit
import qualified Data.Conduit.List         as CL

data Direction =
  ShibuCode
  | Shibu
  | BunkaiCode
  | Bunkai
  | Han
  | KName
  | KNumber
  | KKana
  | KSex
  | KGot
  | KLost
  | KBirthday
  | KAddress
  | KPostal
  | KPhone
  | KCellPhone
  | KFax
  | KKind
  | KKyousai
  | KHonbuY
  | KShibuY
  | KBunkaiY
  | KHanY
  | KKokuhoGet
  | KKokuhoLost
  | Owner
  | KWork
  | HihoOfficeCode
  | HihoOfficeName
  | HihoName
  | HihoKana
  | HihoBirthday
  | HihoGot
  | HihoLost
  | OfficeGot
  | OfficeLost
  | OfficeCode
  | OfficeName
  | OfficeType
  | OfficeRosaiCode
  | OfficeRosaiNumber
  | OfficeKoyouNumber
  | OfficePostal
  | OfficeAddress
  | OfficeTel
  | OfficeFax
  | RawString Text
  | MaybeString (Maybe Text)
  | KillBlanks Direction
  deriving (Eq)

data Figure = Figure { runKumiai :: Maybe K.Kumiai
                     , runOffice :: Maybe B.Office
                     , runHiho   :: Maybe H.Hiho
                     , direction :: [Direction]
                     , before    :: [TLB.Builder]
                     , after     :: [TLB.Builder] } deriving (Eq)

newtype FigureSorterHY = FSH { runFigureHY :: Figure } deriving (Eq)
newtype FigureSorterSY = FSS { runFigureSY :: Figure } deriving (Eq)
newtype FigureSorterBY = FSB { runFigureBY :: Figure } deriving (Eq)
-- data FigureSorterHY = FSH { runFigureHY :: Figure
--                           , hdirections  :: [Direction]}
-- data FigureSorterSY = FSS { runFigureSY :: Figure
--                           , sdirections  :: [Direction]}
-- data FigureSorterBY = FSB { runFigureBY :: Figure
--                           , bdirections  :: [Direction]}
-- data FigureSorterX = FSH Figure [Direction]
--                    | FSS Figure [Direction]
--                    | FSB Figure [Direction]
--                    deriving (Eq)

-- instance Ord FigureSorterX where
--   x `compare` y = x `cmp` y
--     where
--       cmp = mconcat (map directionToSorter $ toDirection x)
class FigureSorter a where
  toFigure :: a -> Figure
  toKumiai :: a -> K.Kumiai

  toKumiai = fromJust . runKumiai . toFigure
-- --------------------------------------------------
instance FigureSorter FigureSorterHY where toFigure = runFigureHY
instance FigureSorter FigureSorterSY where toFigure = runFigureSY
instance FigureSorter FigureSorterBY where toFigure = runFigureBY

-- instance Ord FigureSorterHY where
--   compare = mconcat $ map directionToSorter directions
--     where
--       sortL = [KHonbuY, ShibuCode, KBirthday]

-- toDirection :: FigureSorterX -> [Direction]
-- toDirection (FSH _ d) = d
-- toDirection (FSS _ d) = d
-- toDirection (FSB _ d) = d

-- toKumiai :: FigureSorterX -> K.Kumiai
-- toKumiai (FSH f _) = fromJust $ runKumiai f
-- toKumiai (FSS f _) = fromJust $ runKumiai f
-- toKumiai (FSB f _) = fromJust $ runKumiai f
-- compareWithKumiai :: FigureSorter a => Ord b
--   => K.Kumiai -> b -> (a -> a -> Ordering)
compareWithKumiai f = comparing (f . toKumiai)

directionToSorter :: FigureSorter a => Direction -> (a -> a -> Ordering)
directionToSorter KHonbuY    = compareWithKumiai K.kHonbuY
directionToSorter ShibuCode  = compareWithKumiai K.kShibuCode
directionToSorter KBirthday  = compareWithKumiai K.kBirthday
directionToSorter KShibuY    = compareWithKumiai K.kShibuY
directionToSorter BunkaiCode = compareWithKumiai K.kBunkaiCode
directionToSorter KBunkaiY   = compareWithKumiai K.kBunkaiY
directionToSorter Han        = compareWithKumiai K.kHan

-- instance Ord FigureSorterHY where
--   compare x y = (f x `compare` f y) <> (g x `compare` g y) <> (h x `compare` h y)
--     where
--       f = K.kHonbuY . sorterToKumiai toFigure
--       g = K.kShibuCode . sorterToKumiai toFigure
--       h = K.kBirthday . sorterToKumiai toFigure
--   x `compare` y = sortL
--     where
--       sortL = mconcat
--               [ comparing K.kHonbuY
--               , comparing K.kShibuCode
--               , comparing K.kBirthday]

-- instance Ord FigureSorterSY where
--   compare x y = (f x `compare` f y) <> (g x `compare` g y) <> (h x `compare` h y)
--     where
--       fromKumiai = fromJust . runKumiai . runFigureSY
--       f = K.kShibuY . fromKumiai
--       g = K.kShibuCode . fromKumiai
--       h = K.kBunkaiCode . fromKumiai

-- instance Ord FigureSorterBY where
--   compare x y = (f x `compare` f y) <> (g x `compare` g y) <> (h x `compare` h y)
--     where
--       fromKumiai = fromJust . runKumiai . runFigureBY
--       f = K.kBunkaiY . fromKumiai
--       h = K.kBunkaiCode . fromKumiai
--       g = K.kHan . fromKumiai

translate :: Direction -> Figure -> TLB.Builder
translate ShibuCode         f = (>>->>)      $ K.kShibuCode     <$> runKumiai f
translate Shibu             f = (>>->>)      $ K.kShibu         <$> runKumiai f
translate BunkaiCode        f = (>>->>)      $ K.kBunkaiCode    <$> runKumiai f
translate Bunkai            f = (>>->>)      $ K.kBunkai        <$> runKumiai f
translate Han               f = (>>->>)      $ K.kHan           <$> runKumiai f
translate KName             f = (>>->>)      $ K.kName          <$> runKumiai f
translate KNumber           f = (>>->>)      $ K.kNumber        <$> runKumiai f
translate KKana             f = (>>->>)      $ K.kKana          <$> runKumiai f
translate KSex              f = (>>->>)      $ K.kSex           <$> runKumiai f
translate KAddress          f = (>>->>)      $ K.kAddress       <$> runKumiai f
translate KPostal           f = (>>->>)      $ K.kPostal        <$> runKumiai f
translate KPhone            f = (>>->>)      $ K.kPhone         <$> runKumiai f
translate KCellPhone        f = (>>->>)      $ K.kCellPhone     <$> runKumiai f
translate KFax              f = (>>->>)      $ K.kFax           <$> runKumiai f
translate KKind             f = (>>->>)      $ K.kKind          <$> runKumiai f
translate KKyousai          f = (>>->>)      $ K.kKyousai       <$> runKumiai f
translate KHonbuY           f = (>>->>) $ K.hyToString <$> (K.kHonbuY =<< runKumiai f)
translate KShibuY           f = (>>->>) $ K.syToString <$> (K.kShibuY =<< runKumiai f)
translate KBunkaiY          f = (>>->>) $ K.byToString <$> (K.kBunkaiY =<< runKumiai f)
translate KHanY             f = (>>->>)      $ K.kHanY          =<< runKumiai f
translate Owner             f = (>>->>)      $ B.owner          <$> runOffice f
translate KWork             f = (>>->>)      $ K.kWork          <$> runKumiai f
translate OfficeName        f = (>>->>)      $ B.officeName     <$> runOffice f
translate OfficeCode        f = (>>->>)      $ B.officeCode     <$> runOffice f
translate OfficeType        f = (>>->>)      $ B.officeType     <$> runOffice f
translate OfficeRosaiCode   f = (>>->>)      $ B.rosaiCode      <$> runOffice f
translate OfficeRosaiNumber f = (>>->>)      $ B.rosaiNumber    <$> runOffice f
translate OfficeKoyouNumber f = (>>->>)      $ B.koyouNumber    <$> runOffice f
translate OfficePostal      f = (>>->>)      $ B.officePostal   <$> runOffice f
translate OfficeAddress     f = (>>->>)      $ B.officeAd       <$> runOffice f
translate OfficeTel         f = (>>->>)      $ B.officeTel      <$> runOffice f
translate OfficeFax         f = (>>->>)      $ B.officeFax      <$> runOffice f
translate HihoOfficeCode    f = (>>->>)      $ H.hihoOfficeCode <$> runHiho f
translate HihoOfficeName    f = (>>->>)      $ H.hihoOfficeName <$> runHiho f
translate HihoName          f = (>>->>)      $ H.hihoName       <$> runHiho f
translate HihoKana          f = (>>->>)      $ H.hihoKana       <$> runHiho f
translate KGot              f = maybeBuilder $ K.kGot           =<< runKumiai f
translate KLost             f = maybeBuilder $ K.kLost          =<< runKumiai f
translate HihoGot           f = maybeBuilder $ H.got            =<< runHiho f
translate HihoLost          f = maybeBuilder $ H.lost           =<< runHiho f
translate HihoBirthday      f = maybeBuilder $ H.hihoBirthday   =<< runHiho f
translate KBirthday         f = maybeBuilder $ K.kBirthday     =<< runKumiai f
translate OfficeGot         f = maybeBuilder $ B.officeGot      =<< runOffice f
translate OfficeLost        f = maybeBuilder $ B.officeLost     =<< runOffice f
-- --------------------------------------------------
translate (RawString s)     _ = TLB.fromText s
translate (KillBlanks d)    f = TLB.fromText
                                $ B.killBlanks
                                $ toText
                                $ translate d f
translate (MaybeString d)   _ = (>>->>) d

title :: Direction -> Text
title ShibuCode         = "支部コード"
title Shibu             = "支部"
title BunkaiCode        = "分会コード"
title Bunkai            = "分会"
title Han               = "班"
title KName             = "組合員氏名"
title KNumber           = "組合員番号"
title KKana             = "組合員カナ"
title KSex              = "性別"
title Owner             = "事業主"
title KWork             = "組合員職種"
title OfficeName        = "事業所名"
title OfficeCode        = "事業所コード"
title OfficeType        = "事業所種別"
title OfficeRosaiCode   = "事業所枝番"
title OfficeRosaiNumber = "事業所労災番号"
title OfficeKoyouNumber = "事業所雇用番号"
title OfficePostal      = "事業所郵便番号"
title OfficeAddress     = "事業所住所"
title OfficeTel         = "事業所電話番号"
title OfficeFax         = "事業所FAX"
title HihoOfficeCode    = "被保険者事業所コード"
title HihoOfficeName    = "被保険者事業所名"
title HihoName          = "被保険者名"
title HihoKana          = "被保険者フリガナ"
title KGot              = "組合資格取得日"
title KLost             = "組合資格喪失日"
title HihoGot           = "被保険者資格取得日"
title HihoLost          = "被保険者資格喪失日"
title HihoBirthday      = "被保険者生年月日"
title KBirthday         = "組合員生年月日"
title OfficeGot         = "事業所設置日"
title OfficeLost        = "事業所委託解除日"
title (RawString _)     = "オプション"
title (KillBlanks d)    = title d
title (MaybeString d)   = fromMaybe mempty d

(>>>>) :: Monoid a => (a1 -> a) -> Maybe a1 -> a
(>>>>) f x = mempty `fromMaybe` (f <$> x)

(>>->>) :: Maybe Text -> TLB.Builder
(>>->>) = (TLB.fromText >>>>)

maybeBuilder :: Show a => Maybe a -> TLB.Builder
maybeBuilder Nothing  = mempty
maybeBuilder (Just a) = TLB.fromText $ maybeString (Just a)

maybeString :: Show a => Maybe a -> Text
maybeString (Just a) = pack $ show a
maybeString Nothing  = mempty

toText :: TLB.Builder -> Text
toText = TL.toStrict . TLB.toLazyText

builderConcat :: [TLB.Builder] -> Text
builderConcat []  = mempty
builderConcat tlb = toText
                    $ loop (Prelude.tail tlb) (head tlb <> comma)
  where comma = TLB.singleton ','
        loop [] rest     = rest
        loop [x] rest    = rest <> x
        loop (x:xs) rest = loop xs (rest <> x <> comma)

joinPrint :: [TLB.Builder] -> IO ()
joinPrint = Txio.putStrLn . builderConcat

toBuilder :: Figure -> [TLB.Builder]
toBuilder f = before f <> core <> after f
  where core = [translate d f | d <- direction f]

figurePrint :: Figure -> IO ()
figurePrint = joinPrint . toBuilder

figureSink :: Sink Figure IO ()
figureSink = CL.mapM_ figurePrint

figureMaybeConduit :: MaybeT (ConduitM a Figure IO) Figure
  -> Conduit a IO Figure
figureMaybeConduit c = do
  fig <- runMaybeT c
  forM_ fig yield

