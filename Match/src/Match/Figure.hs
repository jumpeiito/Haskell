module Match.Figure where

import           Control.Lens
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Match.Base                as B
import qualified Match.Hiho                as H
import qualified Match.Kumiai              as K
import           Data.Ord                  (comparing)
import           Data.Foldable             (forM_)
import           Data.Maybe                (fromMaybe, fromJust)
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
                     , runHiho   :: Maybe H.HihoR
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
compareWithKumiai ::
  (FigureSorter a, Ord a1) => (K.Kumiai -> a1) -> a -> a -> Ordering
compareWithKumiai f = comparing (f . toKumiai)

directionToSorter :: FigureSorter a => Direction -> (a -> a -> Ordering)
directionToSorter KHonbuY    = compareWithKumiai (^. #honbuY)
directionToSorter ShibuCode  = compareWithKumiai (^. #shibuCode)
directionToSorter KBirthday  = compareWithKumiai (^. #birth)
directionToSorter KShibuY    = compareWithKumiai (^. #shibuY)
directionToSorter BunkaiCode = compareWithKumiai (^. #bunkaiCode)
directionToSorter KBunkaiY   = compareWithKumiai (^. #bunkaiY)
directionToSorter Han        = compareWithKumiai (^. #han)

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
translate ShibuCode         f = (>>->>)      $ (^. #shibuCode)     <$> runKumiai f
translate Shibu             f = (>>->>)      $ (^. #shibu)         <$> runKumiai f
translate BunkaiCode        f = (>>->>)      $ (^. #bunkaiCode)    <$> runKumiai f
translate Bunkai            f = (>>->>)      $ (^. #bunkai)        <$> runKumiai f
translate Han               f = (>>->>)      $ (^. #han)           <$> runKumiai f
translate KName             f = (>>->>)      $ (^. #name)          <$> runKumiai f
translate KNumber           f = (>>->>)      $ (^. #number)        <$> runKumiai f
translate KKana             f = (>>->>)      $ (^. #kana)          <$> runKumiai f
translate KSex              f = (>>->>)      $ (^. #sex)           <$> runKumiai f
translate KAddress          f = (>>->>)      $ (^. #address)       <$> runKumiai f
translate KPostal           f = (>>->>)      $ (^. #postal)        <$> runKumiai f
translate KPhone            f = (>>->>)      $ (^. #phone)         <$> runKumiai f
translate KCellPhone        f = (>>->>)      $ (^. #cellPhone)     <$> runKumiai f
translate KFax              f = (>>->>)      $ (^. #fax)           <$> runKumiai f
translate KKind             f = (>>->>)      $ (^. #kind)          <$> runKumiai f
translate KKyousai          f = (>>->>)      $ (^. #kyousai)       <$> runKumiai f
translate KHonbuY           f = (>>->>) $ K.hyToString <$> ((^. #honbuY) =<< runKumiai f)
translate KShibuY           f = (>>->>) $ K.syToString <$> ((^. #shibuY) =<< runKumiai f)
translate KBunkaiY          f = (>>->>) $ K.byToString <$> ((^. #bunkaiY) =<< runKumiai f)
translate KHanY             f = (>>->>)      $ (^. #hanY)          =<< runKumiai f
translate Owner             f = (>>->>)      $ (^. #owner)         <$> runOffice f
translate KWork             f = (>>->>)      $ (^. #work)          <$> runKumiai f
translate OfficeName        f = (>>->>)      $ (^. #name)          <$> runOffice f
translate OfficeCode        f = (>>->>)      $ (^. #code)          <$> runOffice f
translate OfficeType        f = (>>->>)      $ (^. #otype)         <$> runOffice f
translate OfficeRosaiCode   f = (>>->>)      $ (^. #rosaiCode)     <$> runOffice f
translate OfficeRosaiNumber f = (>>->>)      $ (^. #rosaiNumber)   <$> runOffice f
translate OfficeKoyouNumber f = (>>->>)      $ (^. #koyouNumber)   <$> runOffice f
translate OfficePostal      f = (>>->>)      $ (^. #postal)        <$> runOffice f
translate OfficeAddress     f = (>>->>)      $ (^. #address)       <$> runOffice f
translate OfficeTel         f = (>>->>)      $ (^. #tel)           <$> runOffice f
translate OfficeFax         f = (>>->>)      $ (^. #fax)           <$> runOffice f
translate HihoOfficeCode    f = (>>->>)      $ (^. #officeCode)    <$> runHiho f
translate HihoOfficeName    f = (>>->>)      $ (^. #officeName)    <$> runHiho f
translate HihoName          f = (>>->>)      $ (^. #name)          <$> runHiho f
translate HihoKana          f = (>>->>)      $ (^. #kana)          <$> runHiho f
translate KGot              f = maybeBuilder $ (^. #got)           =<< runKumiai f
translate KLost             f = maybeBuilder $ (^. #lost)          =<< runKumiai f
translate HihoGot           f = maybeBuilder $ (^. #got)           =<< runHiho f
translate HihoLost          f = maybeBuilder $ (^. #lost)          =<< runHiho f
translate HihoBirthday      f = maybeBuilder $ (^. #birth)         =<< runHiho f
translate KBirthday         f = maybeBuilder $ (^. #birth)         =<< runKumiai f
translate OfficeGot         f = maybeBuilder $ (^. #got)           =<< runOffice f
translate OfficeLost        f = maybeBuilder $ (^. #lost)          =<< runOffice f
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
