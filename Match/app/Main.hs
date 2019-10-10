{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Main where

import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Arrow             ((>>>), (&&&))
import           Control.Applicative       ((<|>))
import           Control.Lens
import           Control.Monad             (when, guard, forM_, liftM)
import           Control.Monad.Except      (ExceptT, runExceptT, liftIO)
import qualified Data.ByteString.Lazy      as BL
import           Data.List                 (sortBy, sort)
import           Data.List.Split           (chunksOf)
import           Data.Ord                  (comparing)
import           Data.Conduit              (Source
                                           , Conduit
                                           , Sink
                                           , awaitForever
                                           , yield
                                           , runConduit
                                           , sourceToList
                                           , (.|)
                                           , ($=), ($$))
import qualified Data.Conduit.Combinators  as CC
import qualified Data.Conduit.List         as CL
import qualified Data.Map.Strict           as M
import           Data.Maybe                (isNothing, isJust, fromMaybe, fromJust)
import           Data.Monoid               ((<>))
import           Data.Time                 (fromGregorian)
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as Tx
import           Data.Text.Lazy.Builder    (Builder, fromText)
import           Data.Time.Clock.POSIX
import           Match.Directory           (createHihoDirectory
                                           , removeBlankDirectory)
import           Match.Figure
import qualified Match.Base                as B
import qualified Match.Hiho                as H
import qualified Match.Hitori              as HT
import qualified Match.Kumiai              as K
import qualified Match.KumiaiOffice        as KO
import qualified Match.Office              as O
import qualified Match.OfficeSP            as OSP
import qualified Match.SQL                 as S
import qualified Match.Map                 as MP
import qualified Options.Applicative       as Q
import qualified System.IO                 as I
import qualified System.Directory          as SD
import           Text.Heredoc
import           Util                      (toCSV)
import           Util.Strdt

makeKey :: K.Kumiai -> Text
makeKey = Tx.take 6 . B.makeKey 7 . (^. #number)

toString :: Figure -> [Text]
toString = map toText . toBuilder

(<??>) :: Ord k => k -> M.Map k a -> Maybe a
(<??>) = M.lookup

run :: ExceptT String IO () -> IO ()
run = (>> return ()) . runExceptT

thisNendo :: IO Integer
thisNendo = toInteger <$> nendo <$> todayDay

-- data Act =
--   MakeCSV FilePath Target
--   | Print Target
--   | MakeLabel FilePath Target

-- data Target = Target Filtering [K.Kumiai]

-- data Filtering = undefined
test :: Bool -> IO ()
test flag = do
  I.hSetEncoding I.stdout I.utf8

  let func =
        if flag
        then H.hihoAliveP
        else (const True)

  runConduit $
    (S.initializeSource :: Source IO H.HihoR)
    .| H.alienFilterConduit
    .| CL.filter func
    .| H.textConduit
    .| CL.mapM_ Tx.putStrLn

test2 :: IO ()
test2 = do
  I.hSetEncoding I.stdout I.utf8

  m <- MP.hitoriRosaiCodeMap
  m2 <- MP.hitoriRosaiCodeMap2

  mainSource <-
    (comparing OSP.KikanBango `sortBy`) <$>
      sourceToList S.initializeSource

  let mainConduit =
        awaitForever $ \osp -> do
          let key = OSP.rosaiNumberKey osp
          let tok = mempty `fromMaybe` (key `M.lookup` m)
          yield $ toCSV [ OSP.toText2 osp, tok]

  -- let
  --     sheet = def & cellValueAt (1,2) ?~ CellDouble (42.0 + 1.0)
  --                 & cellValueAt (3,2) ?~ CellText "foo"
  --     xlsx = def & atSheet "List1" ?~ sheet
  -- BL.writeFile "example.xlsx" $ fromXlsx ct xlsx
  -- let excelSink =
  --       CL.map 

  -- let debugConduit =
  --       awaitForever $ \osp -> do
  --         let key = OSP.rosaiNumberKey osp
  --         case key `M.lookup` m2 of
  --           Just tok -> yield $ toCSV $ tok ^. #personsRow
  --           Nothing  -> return ()

  runConduit
    $ (CL.sourceList mainSource :: Source IO OSP.OfficeSP)
    .| OSP.filter026Conduit
    .| mainConduit
    .| CL.mapM_ Tx.putStrLn

test3 :: IO ()
test3 = do
  I.hSetEncoding I.stdout I.utf8

  n <- thisNendo
  -- 事業所番号にひもづけられた被保険者情報
  om <- MP.hihoKoyouNumberCMap

  officeList <- (S.initializeSource :: Source IO OSP.OfficeSP)
                $$ CL.consume
  let outputFile = ".hiho3.csv"

  let mainConduit =
        awaitForever $ \o -> do
          let officePart = OSP.outputForNendo o
          -- 雇用保険事業所番号にひもづけられた
          -- 被保険者情報を取得する。
          case (o ^. #koyouNumber) `M.lookup` om of
            -- 該当の事業所に被保険者がいない場合,
            -- (該当年度以前に退職している被保険者はカウント
            -- しない),事業所情報部分だけを出力する。
            Nothing -> do
              yield officePart
            Just x  -> do
              -- 該当の事業所番号にひもづけられた被保険者を個人番号
              -- の順番に並びかえる。
              let hiho   = comparing H.IdNumber `sortBy` x
              let sorted = map (H.outputForNendo n) hiho
              -- 被保険者10名ずつ1行に出力する。
              -- 具体的には、
              -- (事業所情報) + (被保険者1番〜10番)
              -- (事業所情報) + (被保険者11番〜20番)
              -- (事業所情報) + (被保険者21番〜30番) …
              -- 上記において、事業所情報は全て同じである。
              forM_ (chunksOf 10 sorted) $ \hihoUnit -> do
                -- yield $ toCSV [officePart, toCSV hihoUnit]
                yield $ officePart ++ (Prelude.concat hihoUnit)

  CL.sourceList (comparing OSP.KikanBango `sortBy` officeList)
    $= CL.filter OSP.koyoP
    $= mainConduit
    $$ CL.mapM_ (toCSV >>> Tx.putStrLn)

test4 :: IO ()
test4 = do
  I.hSetEncoding I.stdout I.utf8

  n <- thisNendo
  -- 事業所番号にひもづけられた被保険者情報
  om <- MP.hihoKoyouNumberCMap

  officeList <- (S.initializeSource :: Source IO OSP.OfficeSP)
                $$ CL.consume
  let outputFile = ".hiho3.csv"

  ct <- getPOSIXTime
  td <- todayDay

  let
    topDirectory  = "d:/home/temp/Haskell/Match/"
    filePreParts  = "雇用保険被保険者基本台帳"
    filePostParts = "現在.xlsx"
    baseFile      =
      Tx.unpack
        (topDirectory <> filePreParts <> "20190315" <> filePostParts)
    newFile       =
      Tx.unpack
        (topDirectory <> filePreParts <> Tx.pack (dayStr8 td) <> filePostParts)

  SD.copyFile baseFile newFile

  let mainConduit =
        awaitForever $ \o -> do
          let officePart = OSP.outputForNendo o
          -- 雇用保険事業所番号にひもづけられた
          -- 被保険者情報を取得する。
          case (o ^. #koyouNumber) `M.lookup` om of
            -- 該当の事業所に被保険者がいない場合,
            -- (該当年度以前に退職している被保険者はカウント
            -- しない),事業所情報部分だけを出力する。
            Nothing -> do
              yield officePart
            Just x  -> do
              -- 該当の事業所番号にひもづけられた被保険者を個人番号
              -- の順番に並びかえる。
              let hiho   = comparing H.IdNumber `sortBy` x
              let sorted = map (H.outputForNendo n) hiho
              -- 被保険者10名ずつ1行に出力する。
              -- 具体的には、
              -- (事業所情報) + (被保険者1番〜10番)
              -- (事業所情報) + (被保険者11番〜20番)
              -- (事業所情報) + (被保険者21番〜30番) …
              -- 上記において、事業所情報は全て同じである。
              forM_ (chunksOf 10 sorted) $ \hihoUnit -> do
                -- yield $ toCSV [officePart, toCSV hihoUnit]
                yield $ officePart ++ (Prelude.concat hihoUnit)

  -- let excelSink = do
  --       l <- CL.consume

  --       forM_ (zip [3..] l) $ \(idx, row) -> liftIO $ do
  --         sequence [let sheet = def & cellValueAt (idx, col) ?~ CellText cell
  --                       xlsx  = def & atSheet "差し込み" ?~ sheet
  --                   in BL.writeFile newFile $ fromXlsx ct xlsx
  --                  | (col, cell) <- zip [2..] row]

  -- let sinkTextFile =
  --       awaitForever $ \line -> do
  --         liftIO $ Tx.appendFile outputFile (line <> "\n")

  -- SD.removeFile outputFile
  let
    excelCell txt = def { _cellValue = Just (CellText txt)}
    excelSink = do
      l <- CL.consume

      let eMap = M.fromList [((ridx, cidx), excelCell v)
                            | (ridx, row) <- zip [3..] l
                            , (cidx, v) <- zip [2..] row]
      -- let ownSheet = def { _wsCells = eMap } :: Worksheet
      -- let ownxlsx  = def { _xlSheets = [("insert", ownSheet)] } :: Xlsx
      -- liftIO $ BL.writeFile newFile $ fromXlsx ct ownxlsx
      bs <- liftIO $ BL.readFile newFile
      let xlsx  = toXlsx bs
      let (Just sheet) = xlsx ^? ixSheet "差し込み"
      let ownSheet = sheet { _wsCells = eMap }
      let ownxlsx  = xlsx { _xlSheets = [("差し込み", ownSheet)]}
      liftIO $ BL.writeFile "temp.xlsx" $ fromXlsx ct ownxlsx


  CL.sourceList (comparing OSP.KikanBango `sortBy` officeList)
    $= CL.filter OSP.koyoP
    $= mainConduit
    -- $$ CL.mapM_ (toCSV >>> Tx.putStrLn)
    $$ excelSink
    -- $$ sinkTextFile

test9 :: IO ()
test9 = do
  let filename = "c:/Users/jumpei/Dropbox/2019年度　出勤簿（原紙）.xlsx"
  bs <- liftIO $ BL.readFile filename
  let xlsx = toXlsx bs
  forM_ ([4..12] ++ [1..3]) $ \month -> do
    readFromFile xlsx month

test10 :: IO ()
test10 = do
  I.hSetEncoding I.stdout I.utf8
  hm   <- MP.hitori2NumberMap
  hkmb <- MP.hihoKanaShibuBirthCMap
  om   <- MP.ospCodeAllCMap
  okm  <- MP.ospKoyouNumberCMap

  let startDay = fromGregorian 2019 4 1

  let hihoP mp k =
        let key k = (B.regularize (k ^. #kana), k ^. #shibuCode, k ^. #birth)
        in case key k `M.lookup` mp of
          Just [h] | H.hihoAliveP h -> Just h
                   | otherwise      -> Nothing
          _                         -> Nothing

  let hitoriO mp k = case ((6 `Tx.take` (k ^. #number)) `M.lookup` mp) of
                       Just h | HT.hitoriOLiveP h -> Just h
                              | otherwise         -> Nothing
                       Nothing                    -> Nothing

  let officeP mp k = (6 `Tx.take` (k ^. #number)) `M.lookup` mp

  let judge k =
        case (K.kokuhoNewer startDay k, hihoP hkmb k, hitoriO hm k, officeP om k) of
          --------------------------------------------------
          -- 2019年度に加入した場合
          --------------------------------------------------
          (True, _, _, _)   ->
            let kokuhoGet = Tx.pack $ show $ fromJust (k ^. #kokuhoGet)
            in ([heredoc|今年度加入(${kokuhoGet})|], k)
          --------------------------------------------------
          -- 雇用保険被保険者
          --------------------------------------------------
          (_, Just h, _, _) ->
            case (h ^. #koyouNumber) `M.lookup` okm of
              Just osp ->
                let mainPart = H.kokuhoOutput h
                    officePart = OSP.kokuhoOutput2 osp
                in ([heredoc|${mainPart},${officePart}|], k)
              Nothing  -> (H.kokuhoOutput h, k)
          --------------------------------------------------
          -- 一人親方労災加入者
          --------------------------------------------------
          (_, _, Just h, _) -> (HT.kokuhoOutput h, k)
          --------------------------------------------------
          -- 労働保険加入者
          --------------------------------------------------
          (_, _, _, Just h) ->
            let mainPart = OSP.kokuhoOutput h
                subPart  = OSP.kokuhoOutput2 h
            in ([heredoc|${mainPart},${subPart}|], k)
          (_, _, _, _)      -> ("", k)

      -- case (K.kokuhoAliveP k, hihoP hkmb k, hitoriO hm k, officeP om k) of
      --     (False, _, _, _)  -> ("国保未加入", k)
      --     (_, Just h, _, _) ->
      --       case (h ^. #koyouNumber) `M.lookup` okm of
      --         Just osp ->
      --           let mainPart = H.kokuhoOutput h
      --               officePart = OSP.kokuhoOutput2 osp
      --           in ([heredoc|${mainPart},${officePart}|], k)
      --         Nothing  -> (H.kokuhoOutput h, k)
      --     (_, _, Just h, _) -> (HT.kokuhoOutput h, k)
      --     (_, _, _, Just h) ->
      --       let mainPart = OSP.kokuhoOutput h
      --           subPart  = OSP.kokuhoOutput2 h
      --       in ([heredoc|${mainPart},${subPart}|], k)
      --     (_, _, _, _)      -> ("", k)

  let str (s, k) =
        let symlist = [ k ^. #kokuhoNumber
                      , k ^. #shibuCode
                      , k ^. #shibu
                      , k ^. #bunkaiCode
                      , k ^. #bunkai
                      , k ^. #workCode
                      , k ^. #work
                      , k ^. #name
                      , k ^. #klassCode
                      , k ^. #klass
                      , ""
                      , k ^. #office
                      , "", "", "", ""
                      , s]
        in let join = Tx.intercalate ","
        in join symlist

  runConduit $
    (S.initializeSource :: Source IO K.Kumiai)
    .| CL.filter K.kokuhoAliveP
    .| CL.mapM_ (judge >>> str >>> Tx.putStrLn)

test11 :: String -> IO ()
test11 s = do
  -- I.hSetEncoding I.stdout I.utf8

  kmb <- MP.kumiaiBirthdayNameCMap
  knm <- MP.kumiaiNumberCMap

  let hihoKey h = ( B.regularize $ B.killBlanks $ h ^. #kana
                  , h ^. #birth)

  let toText d = mempty `fromMaybe` ((Tx.pack . show) <$> d)

  let mainPart h =
        let _name  = h ^. #name
            _birth = toText (h ^. #birth)
            _got   = toText (h ^. #got)
            _oname = h ^. #officeName
            _ocode = h ^. #officeCode
        in case _ocode `M.lookup` knm of
             Nothing -> ""
             Just k ->
               Tx.intercalate "," [ k ^. #bunkaiCode <> k ^. #han
                                  , _oname
                                  , k ^. #bunkai
                                  , k ^. #han
                                  , k ^. #name
                                  , _name
                                  , _birth
                                  , _got]

  let subPart h =
        case hihoKey h `M.lookup` kmb of
          Nothing  -> mempty
          Just [k] ->
            let _bunkai = k ^. #bunkai
                _han    = k ^. #han
                _shibu  = k ^. #shibu
            in [heredoc|${_shibu}支部,${_bunkai}分会,${_han}班|]

  let output h =
        let m = mainPart h
            s = subPart h
        in [heredoc|${m},${s}|]

  finalizer <-
    runConduit $
    (S.initializeSource :: Source IO H.HihoR)
    .| CL.filter H.hihoAliveP
    .| CL.filter ((^. #shibu) >>> (== Just (Tx.pack s)))
    .| CL.map (output &&& id)
    .| CL.consume

  -- forM_ (sort finalizer) Tx.putStrLn
  forM_ (sort finalizer) print

toMonthString :: Int -> Text
toMonthString n | n < 10 = Tx.pack $ "0" ++ (show n) ++ "月"
          | otherwise = Tx.pack $ show n ++ "月"

readFromFile :: Xlsx -> Int -> IO ()
readFromFile xlsx i = do
  let v = [map (\n -> xlsx ^? ixSheet (toMonthString i) . ixCell (x, n) . cellValue . _Just) [1..20]
          | x <- [8..38]]
  mapM_ print v

----------------------------------------------------------------------
jigyosyoMatchUp :: IO ()
jigyosyoMatchUp = do
  o  <- MP.officeNumberMap
  ko <- MP.koNumberMap

  (S.initializeSource                    :: Source IO K.Kumiai)
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
  (S.initializeSource                   :: Source IO B.Office)
    $= (CL.filter O.numberInfixAddressP :: Conduit B.Office IO B.Office)
    $= (CL.map O.basicInfo              :: Conduit B.Office IO [Builder])
    $$ (CL.mapM_ (liftIO . joinPrint)   :: Sink [Builder] IO ())

----------------------------------------------------------------------
hihoNameMatchUp :: IO ()
hihoNameMatchUp = do
  bMap <- MP.kumiaiBirthdayCMap

  (S.initializeSource                   :: Source IO H.HihoR)
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
  bMap <- MP.kumiaiBirthdayNameCMap

  (S.initializeSource                   :: Source IO H.HihoR)
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
  bMap <- MP.kumiaiBirthdayNameCMap

  (S.initializeSource :: Source IO H.HihoR)
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
  kMap <- MP.kumiaiOfficeCodeMap

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
    $ S.initializeSource
    .| CL.filter ((^. #ownerName) >>> (== ""))
    .| CL.map (id &&& ((^. #code) >>> (`mapSearch` kMap)))
    .| CL.filter (\(_, l) -> not (null l))
    .| CL.map (\(ko, l) -> (map toText (KO.stringList ko), l))
    .| CL.map (\(ko, l) -> map (kumiaiInfo >>> (ko++) >>> Tx.intercalate ",") l)
    .| CL.mapM_ (mapM_ Tx.putStrLn)
----------------------------------------------------------------------
shibuMatchUp :: Text -> IO ()
shibuMatchUp s = do
  kMap <- MP.kumiaiBirthdayNameCMap
  oMap <- MP.officeNumberMap

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

  (S.initializeSource     :: Source IO H.HihoR)
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
  kb <- MP.hihoKanaBirthCMap
  ko <- MP.koNumberCMap

  (S.initializeSource  :: Source IO K.Kumiai)
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
  telMap <- MP.officeTelMap
  numMap <- MP.kumiaiNumberCMap

  (S.initializeSource        :: Source IO KO.KumiaiOffice)
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
--   Right offi <- liftIO S.initializeData
--   liftIO $ I.hSetEncoding I.stdout I.utf8

--   _forM_ offi $ \office -> do
--     let ocode = B.officeCode office
--     let oname = B.officeTypeReplace $ B.officeName office
--     Txio.putStrLn $ ocode <> "_" <> oname
simpleOfficeOutput = undefined

----------------------------------------------------------------------
yakuOutput :: IO ()
yakuOutput =
  S.initializeSource
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
  -- sjis <- I.mkTextEncoding "CP932"
  I.hSetEncoding I.stdout I.utf8

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
  when (nendoKousin' opt)    test3
  when (kokuhoOutput' opt)   test10

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
                       , nendoKousin'    :: Bool
                       , kokuhoOutput'   :: Bool
                       , shibu'          :: String
                       } deriving (Show)

hAddressP, jigyosyoP, officeAddressP, hihoNameP, hihoNameStrictP   :: Q.Parser Bool
removeDirP, kumiaiOfficeP, kumiaiP, createDirectoryP, simpleOfficeP :: Q.Parser Bool
kokuhoOutputP :: Q.Parser Bool
yakuOutputP, kumiaiBlankP :: Q.Parser Bool
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
kumiaiBlankP     = Q.switch $ Q.short 'q' <> Q.long "koBlank"   <> Q.help ""
kokuhoOutputP    = Q.switch $ Q.short 'c' <> Q.long "kokuhoOutput"   <> Q.help ""
nendoKousinP     = Q.switch $ Q.short 'z' <> Q.long "nendoKousin"
  <> Q.help "ハローワーク届出の被保険者名簿を出力する。"

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
           <*> nendoKousinP
           <*> kokuhoOutputP
           <*> shibuP

myParserInfo :: Q.ParserInfo Options
myParserInfo = Q.info optionsP $ mconcat
    [ Q.fullDesc
    , Q.progDesc "test program."
    , Q.header "snews.exe -- get a daily news article program."
    , Q.footer ""
    , Q.progDesc ""
    ]
