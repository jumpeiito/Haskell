{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens
import           Control.Monad             (when, guard)
import           Control.Monad.Except      (ExceptT, runExceptT, liftIO)
import           Data.List                 (sortBy)
import           Data.Ord                  (comparing)
import           Data.Conduit              (Source
                                           , Conduit
                                           , Sink
                                           , awaitForever
                                           , yield
                                           , runConduit
                                           , (.|)
                                           , ($=), ($$))
import qualified Data.Conduit.List         as CL
import qualified Data.Map.Strict           as M
import           Data.Maybe                (isNothing, isJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as Tx
import           Data.Text.Lazy.Builder    (Builder, fromText)
import           Match.Directory           (createHihoDirectory
                                           , removeBlankDirectory)
import           Match.Figure
import qualified Match.Base                as B
import qualified Match.Hiho                as H
import qualified Match.Kumiai              as K
import qualified Match.KumiaiOffice        as KO
import qualified Match.Office              as O
import qualified Options.Applicative       as Q
import qualified System.IO                 as I

makeKey :: K.Kumiai -> Text
makeKey = Tx.take 6 . B.makeKey 7 . (^. #number)

toString :: Figure -> [Text]
toString = map toText . toBuilder

(<??>) :: Ord k => k -> M.Map k a -> Maybe a
(<??>) = M.lookup

run :: ExceptT String IO () -> IO ()
run = (>> return ()) . runExceptT

-- data Act =
--   MakeCSV FilePath Target
--   | Print Target
--   | MakeLabel FilePath Target

-- data Target = Target Filtering [K.Kumiai]

-- data Filtering = undefined
----------------------------------------------------------------------
jigyosyoMatchUp :: IO ()
jigyosyoMatchUp = do
  o  <- O.numberMap
  ko <- KO.numberMap

  (K.initializeSource                    :: Source IO K.Kumiai)
    $= (CL.filter (isNothing . (^. #lost)) :: Conduit K.Kumiai IO K.Kumiai)
    $= (conduit o ko                     :: Conduit K.Kumiai IO Figure)
    $$ (figureSink                       :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit o' ko' =
      awaitForever $ \kumiai ->
        figureMaybeConduit $ do
          let key = makeKey kumiai
          Just a <- return $ key `M.lookup` o'
          let w = kumiai ^. #office
          guard $ (a ^. #name) /= w
          let sym | w == ""   = "Blank"
                  | otherwise = "Diff"
          return Figure { runKumiai = Just kumiai
                        , runOffice = Just a
                        , runHiho   = Nothing
                        , before    = [sym]
                        , after     =
                            fromMaybe mempty $
                            KO.stringList <$> key <??> ko'
                        , direction = [ ShibuCode
                                      , Shibu
                                      , BunkaiCode
                                      , Bunkai
                                      , Han
                                      , KName
                                      , KWork
                                      , KillBlanks Owner
                                      , OfficeName
                                      , OfficePostal
                                      , OfficeAddress
                                      , OfficeTel
                                      , OfficeFax]}

----------------------------------------------------------------------
officeAddressMatchUp :: IO ()
officeAddressMatchUp =
  (O.initializeSource                   :: Source IO B.Office)
    $= (CL.filter O.numberInfixAddressP :: Conduit B.Office IO B.Office)
    $= (CL.map O.basicInfo              :: Conduit B.Office IO [Builder])
    $$ (CL.mapM_ (liftIO . joinPrint)   :: Sink [Builder] IO ())

----------------------------------------------------------------------
hihoNameMatchUp :: IO ()
hihoNameMatchUp = do
  bMap <- K.birthdayCMap

  (H.initializeSource                   :: Source IO H.HihoR)
    $= (CL.filter H.hihoNameUnfinishedP :: Conduit H.HihoR IO H.HihoR)
    $= (conduit bMap                    :: Conduit H.HihoR IO Figure)
    $$ (figureSink                      :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit m =
      awaitForever $ \hiho ->
        figureMaybeConduit $ do
          Just xs <- return $ (hiho ^. #birth) `M.lookup` m
          return Figure { runKumiai = Nothing
                        , runOffice = Nothing
                        , runHiho   = Just hiho
                        , before    = mempty
                        , after     =
                            map (fromText . K.verboseName) xs
                        , direction = [ HihoOfficeCode
                                      , HihoOfficeName
                                      , HihoName
                                      , HihoBirthday]}

----------------------------------------------------------------------
hihoNameStrictMatchUp :: IO ()
hihoNameStrictMatchUp = do
  bMap <- K.birthdayNameCMap

  (H.initializeSource                   :: Source IO H.HihoR)
    $= (CL.filter H.hihoNameUnfinishedP :: Conduit H.HihoR IO H.HihoR)
    $= (conduit bMap                    :: Conduit H.HihoR IO Figure)
    $$ (figureSink                      :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit m =
      awaitForever $ \hiho ->
        figureMaybeConduit $ do
          let kana  = B.killBlanks $ hiho ^. #kana
          Just xs <- return $ (kana, hiho ^. #birth) `M.lookup` m
          return Figure { runKumiai = Nothing
                        , runOffice = Nothing
                        , runHiho   = Just hiho
                        , before    = mempty
                        , after     =
                            map (fromText . K.verboseName) xs
                        , direction = [ HihoOfficeCode
                                      , HihoOfficeName
                                      , HihoName
                                      , HihoBirthday]}
----------------------------------------------------------------------
hihoAddressMatchUp :: IO ()
hihoAddressMatchUp = do
  bMap <- K.birthdayNameCMap

  (H.initializeSource :: Source IO H.HihoR)
    $= (CL.filter H.hihoAddressBlankP
                      :: Conduit H.HihoR IO H.HihoR)
    $= (conduit bMap  :: Conduit H.HihoR IO Figure)
    $$ (figureSink    :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit m =
      awaitForever $ \hiho ->
        figureMaybeConduit $ do
          let kana  = B.killBlanks $ hiho ^. #kana
          Just xs <- return $ (kana, hiho ^. #birth) `M.lookup` m
          return Figure { runKumiai = Just $ head xs
                        , runOffice = Nothing
                        , runHiho   = Just hiho
                        , before    = mempty
                        , after     = []
                        , direction = [ HihoOfficeCode
                                      , HihoOfficeName
                                      , KillBlanks HihoName
                                      , HihoBirthday ]}
----------------------------------------------------------------------
kumiaiOfficeBlankMatchUp :: IO ()
kumiaiOfficeBlankMatchUp = do
  kMap <- K.officeCodeMap

  let mapSearch l m = case l `M.lookup` m of Just x -> x; Nothing -> []
  let tp t = Tx.pack (fromMaybe "" $ show <$> t)
  let funcs = [ (^. #shibu)
              , (^. #bunkai)
              , (^. #han)
              , (^. #number)
              , (^. #name)
              , (^. #got) >>> tp
              , (^. #lost) >>> tp]
  let kumiaiInfo k = map ($ k) funcs
  runConduit
    $ KO.initializeSource
    .| CL.filter ((^. #ownerName) >>> (== ""))
    .| CL.map (id &&& ((^. #code) >>> (`mapSearch` kMap)))
    .| CL.filter (\(_, l) -> not (null l))
    .| CL.map (\(ko, l) -> (map toText (KO.stringList ko), l))
    .| CL.map (\(ko, l) -> map (kumiaiInfo >>> (ko++) >>> Tx.intercalate ",") l)
    .| CL.mapM_ (mapM_ Tx.putStrLn)
----------------------------------------------------------------------
shibuMatchUp :: Text -> IO ()
shibuMatchUp s = do
  kMap <- K.birthdayNameCMap
  oMap <- O.numberMap

  joinPrint [ "支部コード"
            , "記号"
            , "事業所コード"
            , "事業所名"
            , "支部"
            , "分会"
            , "班"
            , "被保険者氏名"
            , "被保険者カナ"
            , "生年月日"
            , "雇用保険取得日"
            , "雇用保険喪失日" ]

  (H.initializeSource     :: Source IO H.HihoR)
    $= (CL.filter (\h -> (H.hihoAliveP h) && (H.hihoOfficeAliveP h)))
    $= (CL.filter (\h -> h ^. #shibu == Just s))
    $= (conduit kMap oMap :: Conduit H.HihoR IO Figure)
    $$ (figureSink        :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit k o =
      awaitForever $ \hiho ->
        figureMaybeConduit $ do
          Just offi <- return $ (hiho ^. #officeCode) `M.lookup` o
          let kMatch = (hiho ^. #kana, hiho ^. #birth) `M.lookup` k
          let kumiai = head <$> kMatch
          let sym = case (kumiai, (^. #lost) =<< kumiai , offi ^. #lost) of
                      (Just _, Nothing, Nothing) -> "現組"
                      (Just _, Just _, Nothing)  -> "元組"
                      _                  -> ""
          return Figure { runKumiai = kumiai
                        , runOffice = Nothing
                        , runHiho   = Just hiho
                        , before    = map fromText [s, sym]
                        , after     = mempty
                        , direction = [ HihoOfficeCode
                                      , HihoOfficeName
                                      , Shibu
                                      , Bunkai
                                      , Han
                                      , HihoName
                                      , HihoKana
                                      , HihoBirthday
                                      , HihoGot
                                      , HihoLost]}
----------------------------------------------------------------------
kumiaiinMatchUp :: IO ()
kumiaiinMatchUp = do
  kb <- H.kanaBirthCMap
  ko <- KO.numberCMap

  (K.initializeSource  :: Source IO K.Kumiai)
    $= (CL.filter (\k -> (isNothing (k ^. #lost) && (k ^. #office == "")))
                       :: Conduit K.Kumiai IO K.Kumiai)
    $= (conduit kb ko  :: Conduit K.Kumiai IO Figure)
    $$ (figureSink     :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit b o =
      awaitForever $ \kumiai ->
        figureMaybeConduit $ do
          Just offi <- return (K.kanaBirthKey kumiai `M.lookup` b)
          let alive = filter H.hihoAliveP offi
          guard (not $ Prelude.null alive)
          let hiho  = head alive
          let ocode = case (hiho ^. #officeCode) `M.lookup` o of
                        Just xl -> xl ^. #code
                        Nothing -> ""
          return Figure { runKumiai = Just kumiai
                        , runOffice = Nothing
                        , runHiho   = Just hiho
                        , before    = mempty
                        , after     = mempty
                        , direction = [ Shibu
                                      , Bunkai
                                      , Han
                                      , KNumber
                                      , HihoName
                                      , KillBlanks KWork
                                      , RawString ocode
                                      , KillBlanks HihoOfficeName
                                      , HihoGot
                                      , HihoLost ]}
----------------------------------------------------------------------
kumiaiOfficeMatchUp :: IO ()
kumiaiOfficeMatchUp = do
  telMap <- O.telMap
  numMap <- K.numberCMap

  (KO.initializeSource        :: Source IO KO.KumiaiOffice)
    $= (CL.filter ((^. #idNumber) >>> (==""))
                              :: Conduit KO.KumiaiOffice IO KO.KumiaiOffice)
    $= (conduit telMap numMap :: Conduit KO.KumiaiOffice IO Figure)
    $$ (figureSink            :: Sink Figure IO ())
----------------------------------------------------------------------
  where
    conduit t n =
      awaitForever $ \koffice ->
        figureMaybeConduit $ do
          Just o   <- return $ (koffice ^. #tel) `M.lookup` t
          Just hit <- return $ (o ^. #code) `M.lookup` n
          return Figure { runKumiai = Just hit
                        , runOffice = Just o
                        , runHiho   = Nothing
                        , before    =
                            fromText "tel" :
                            KO.stringList koffice
                        , after     = mempty
                        , direction = [ KNumber
                                      , KName
                                      , KKana
                                      , KGot
                                      , KLost
                                      , OfficePostal
                                      , OfficeAddress
                                      , OfficeTel
                                      , OfficeFax
                                      , OfficeName
                                      , OfficeType]}
----------------------------------------------------------------------
-- simpleOfficeOutput :: IO ()
-- simpleOfficeOutput = run $ do
--   Right offi <- liftIO O.initializeData
--   liftIO $ I.hSetEncoding I.stdout I.utf8

--   _forM_ offi $ \office -> do
--     let ocode = B.officeCode office
--     let oname = B.officeTypeReplace $ B.officeName office
--     Txio.putStrLn $ ocode <> "_" <> oname
simpleOfficeOutput = undefined

----------------------------------------------------------------------
yakuOutput :: IO ()
yakuOutput =
  K.initializeSource
    -- $= CL.filter (K.kShibuCode >>> (=="18"))
    -- $= CL.filter (K.kBunkaiCode >>> (=="03"))
    $= CL.filter ((^. #honbuY) >>> isJust)
    $= conduit
    $$ sink
  where
    conduit =
      awaitForever $ \kumiai ->
        yield Figure { runKumiai = Just kumiai
                     , runOffice = Nothing
                     , runHiho   = Nothing
                     , before    = mempty
                     , after     = mempty
                     , direction = [Shibu
                                   , Bunkai
                                   , Han
                                   , KNumber
                                   , KName
                                   , KKana
                                     -- , KPostal
                                     -- , KAddress
                                     -- , KGot
                                     -- , KLost
                                   -- , KWork
                                   , KHonbuY
                                   , KShibuY
                                   , KBunkaiY
                                   -- , KHanY
                                   ]}
    sink = do
      xl <- CL.consume
      -- let sorter = sortBy (comparing FSH) xl
      mapM_ (liftIO . figurePrint) xl
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
main :: IO ()
main = do
  sjis <- I.mkTextEncoding "CP932"
  I.hSetEncoding I.stdout sjis

  --------------------------------------------------------------------------------

  opt <- Q.customExecParser (Q.prefs Q.showHelpOnError) myParserInfo

  when (hAddress' opt)       hihoAddressMatchUp
  when (jigyosyo' opt)       jigyosyoMatchUp
  when (officeAddress' opt)  officeAddressMatchUp
  when (removeD' opt)        removeBlankDirectory
  when (hihoName' opt)       hihoNameMatchUp
  when (hihoNameStrict' opt) hihoNameStrictMatchUp
  when (simpleOffice' opt)   simpleOfficeOutput
  when (kumiai' opt)         kumiaiinMatchUp
  when (kumiaiO' opt)        kumiaiOfficeMatchUp
  when (createD' opt)        createHihoDirectory
  when (yakuD' opt)          yakuOutput
  when (kumiaiBlank' opt)    kumiaiOfficeBlankMatchUp

  case shibu' opt of
    "" -> return ()
    s  -> shibuMatchUp $ Tx.pack s
  -- --------------------------------------------------------------------------------

data Options = Options { hAddress'       :: Bool
                       , jigyosyo'       :: Bool
                       , officeAddress'  :: Bool
                       , removeD'        :: Bool
                       , hihoName'       :: Bool
                       , hihoNameStrict' :: Bool
                       , simpleOffice'   :: Bool
                       , kumiai'         :: Bool
                       , kumiaiO'        :: Bool
                       , createD'        :: Bool
                       , yakuD'          :: Bool
                       , kumiaiBlank'    :: Bool
                       , shibu'          :: String
                       } deriving (Show)

hAddressP, jigyosyoP, officeAddressP, hihoNameP, hihoNameStrictP   :: Q.Parser Bool
removeDirP, kumiaiOfficeP, kumiaiP, createDirectoryP, simpleOfficeP :: Q.Parser Bool
hAddressP        = Q.switch $ Q.short 'a' <> Q.long "hihoAddress"    <> Q.help ""
jigyosyoP        = Q.switch $ Q.short 'j' <> Q.long "jigyosyo"       <> Q.help ""
officeAddressP   = Q.switch $ Q.short 'o' <> Q.long "officeAddress"  <> Q.help ""
removeDirP       = Q.switch $ Q.short 'r' <> Q.long "removeDir"      <> Q.help ""
hihoNameP        = Q.switch $ Q.short 'n' <> Q.long "hihoName"       <> Q.help ""
hihoNameStrictP  = Q.switch $ Q.short 'x' <> Q.long "hihoNameStrict" <> Q.help ""
kumiaiP          = Q.switch $ Q.short 'k' <> Q.long "kumiai"
  <> Q.help "基幹システム中の組合員で、事業所欄が空欄の組合員を検出し、雇用保険がかかっているかどうかを調査する。雇用保険の資格を持っている場合、雇用保険データを表示。"
kumiaiOfficeP    = Q.switch $ Q.short 'l' <> Q.long "kumiaiOffice"   <> Q.help ""
createDirectoryP = Q.switch $ Q.short 'm' <> Q.long "createDirectory" <> Q.help ""
simpleOfficeP    = Q.switch $ Q.short 'y' <> Q.long "simpleOffice"   <> Q.help ""
yakuOutputP      = Q.switch $ Q.short 'b' <> Q.long "yakuOutput "   <> Q.help ""
kumiaiBlankP     = Q.switch $ Q.short 'q' <> Q.long "koBlank "   <> Q.help ""

shibuP :: Q.Parser String
shibuP = Q.strOption $ mconcat
        [ Q.short 's', Q.long "shibu"
        , Q.help ""
        , Q.metavar ""
        , Q.value ""
        , Q.showDefaultWith id ]

optionsP :: Q.Parser Options
optionsP = (<*>) Q.helper
           $ Options
           <$> hAddressP
           <*> jigyosyoP
           <*> officeAddressP
           <*> removeDirP
           <*> hihoNameP
           <*> hihoNameStrictP
           <*> simpleOfficeP
           <*> kumiaiP
           <*> kumiaiOfficeP
           <*> createDirectoryP
           <*> yakuOutputP
           <*> kumiaiBlankP
           <*> shibuP

myParserInfo :: Q.ParserInfo Options
myParserInfo = Q.info optionsP $ mconcat
    [ Q.fullDesc
    , Q.progDesc "test program."
    , Q.header "snews.exe -- get a daily news article program."
    , Q.footer ""
    , Q.progDesc ""
    ]
