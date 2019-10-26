{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Ukyo where

import           Control.Arrow             ((>>>), (&&&))
import           Control.Lens
import           Control.Exception.Safe    (MonadThrow, throwM)
import           Control.Monad             (forM_, when)
import           Control.Monad.Trans       (MonadIO, liftIO, lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson
import           Data.Conduit              (Sink
                                           , Source
                                           , Conduit
                                           , runConduit
                                           , yield
                                           , awaitForever
                                           , (.|))
import           Data.Conduit.Async
import qualified Data.Conduit.List         as CL
import           Data.List                 (intercalate)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe
                                           , fromJust, isNothing)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as Tx
import           Data.Time                 (Day)
import           GHC.Generics              (Generic)
import qualified Match.Base                as B
import           Match.CSV                 (Spec, parseCSVSource)
import           Match.Kumiai
import           Match.TreeMake
import           Match.Geocoder            (makeJavascriptFileKumiai, MakeMap)
import qualified Options.Applicative       as Q
import qualified System.IO                 as I
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Heredoc              (heredoc)
import           Util.Exception            (FileNotExistException (..))
import           Util.Strdt                (todayDay, howOld)
import           Util.Yaml                 (readYaml)

-- $setup
-- >>> :set -XOverloadedStrings
data Config = C { dataCSVFileName    :: FilePath
                , sortCR             :: Bool
                , makeMapCR          :: MakeMap
                , relationalFileName :: FilePath
                , addressRemove      :: [Text]
                , workReplaceAlist   :: [[Text]]
                , printOrder         :: [Direction]
                }
  deriving (Show, Read, Generic)

instance FromJSON Direction
instance FromJSON Config

type UnderConfig a = Reader Config a
type UnderConfigT a b = ReaderT Config a b
type WorkMap = M.Map Text Text

kumiaiComp :: Kumiai -> Text
kumiaiComp k = s k <> b k <> h k <> pt k
  where
    s  = (^. #shibuCode)
    b  = (^. #bunkaiCode)
    h  = (^. #han)
    pt = (^. #printOrder)
         >>> fromJust
         >>> (* 1000)
         >>> show
         >>> Tx.pack
         >>> Tx.takeWhile (/= '.')
         >>> Tx.justifyRight 5 '0'

readConfig :: (MonadThrow m, MonadIO m) => FilePath -> m Config
readConfig = readYaml

spec :: Spec
spec = [ "支部コード"
       , "支部"
       , "分会コード"
       , "分会"
       , "班"
       , "組合員番号"
       , "氏名"
       , "氏名カナ"
       , "性別"
       , "生年月日"
       , "加入日"
       , "脱退日"
       , "職種"
       , "就労先"
       , "就労先コード"
       , "台帳表示順"
       , "電話番号"
       , "携帯番号"
       , "FAX"
       , "郵便番号"
       , "住所"
       , "組合種別"
       , "共済区分"
       , "役職(本部)"
       , "役職(支部)"
       , "役職(分会)"
       , "役職(班)"
       , "資格取得日"
       , "資格喪失日"]

relationSpec :: Spec
relationSpec = [ "組合員番号"
               , "親方"
               , "親方番号"]

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
  | KOffice
  | KPrintOrder
  | BunkaichoMark
  | BunkaiKaikeiMark
  | HanchoMark
  | KokuhoMark
  | Relational
  | Explanation -- equal with "Combine [KokuhoMark, Relational]"
  | HowOld
  | PV
  deriving (Show, Read, Generic)

translate :: Direction -> Kumiai -> Text
translate ShibuCode        k = k ^. #shibuCode
translate Shibu            k = k ^. #shibu
translate BunkaiCode       k = k ^. #bunkaiCode
translate Bunkai           k = k ^. #bunkai
translate Han              k = k ^. #han
translate KName            k = k ^. #name
translate KNumber          k = k ^. #number
translate KKana            k = k ^. #kana
translate KSex             k = k ^. #sex
translate KAddress         k = k ^. #address
translate KPostal          k = k ^. #postal
translate KPhone           k = k ^. #phone
translate KCellPhone       k = k ^. #cellPhone
translate KFax             k = k ^. #fax
translate KKind            k = k ^. #kind
translate KKyousai         k = k ^. #kyousai
translate KWork            k = k ^. #work
translate KOffice          k = k ^. #office
translate KHonbuY          k = (>>->>) $ hyToString <$> k ^. #honbuY
translate KShibuY          k = (>>->>) $ syToString <$> k ^. #shibuY
translate KBunkaiY         k = (>>->>) $ byToString <$> k ^. #bunkaiY
translate KHanY            k = (>>->>) $ k ^. #hanY
translate KPrintOrder      k = (>>->>) $ Tx.pack . show <$> k ^. #printOrder
translate KGot             k = maybeString $ k ^. #got
translate KLost            k = maybeString $ k ^. #lost
translate KBirthday        k = maybeString $ k ^. #birth
translate HowOld           k = makeHowOld  $ k ^. #birth
translate KKokuhoGet       k = maybeString $ k ^. #kokuhoGet
translate KKokuhoLost      k = maybeString $ k ^. #kokuhoLost
translate Owner            _ = ""
translate BunkaichoMark    k = makeBCMark $ byToString <$> k ^. #bunkaiY
translate BunkaiKaikeiMark k = makeBKMark $ byToString <$> k ^. #bunkaiY
translate HanchoMark       k = makeHMark $ k ^. #hanY
translate KokuhoMark       k = makeKokuhoMark k
translate Relational       _ = ""
translate Explanation      k = makeExplanation k
translate PV               k = Tx.pack $ show $ kumiaiComp k

(>>>>) :: Monoid a => (a1 -> a) -> Maybe a1 -> a
(>>>>) f x = mempty `fromMaybe` (f <$> x)

(>>->>) :: Maybe Text -> Text
(>>->>) = (id >>>>)

-- |
-- >>> contains "分会長" "分会長"
-- True
-- >>> contains "班長" "班長"
-- True
-- >>> contains "副分会長" "分会長"
-- False
-- >>> contains "分会長・分会会計" "分会長"
-- True
-- >>> contains "分会長・分会会計" "分会会計"
-- True
contains :: Text -> Text -> Bool
contains t s = s `elem` Tx.splitOn "・" t

makeMark :: Text -> String -> String -> String
makeMark target part ret = if target `Ukyo.contains` Tx.pack part
                           then ret
                           else ""

makeBCMark, makeBKMark, makeHMark :: Maybe Text -> Text
makeBCMark Nothing  = ""
makeBCMark (Just x) = pack $ makeMark x "分会長" "◎"
makeBKMark Nothing  = ""
makeBKMark (Just x) = pack $ makeMark x "分会会計" "◯"
makeHMark Nothing   = ""
makeHMark (Just x)  = pack $ makeMark x "班長" "●"

makeHowOld :: Maybe Day -> Text
makeHowOld Nothing  = ""
makeHowOld (Just d) = Tx.pack . show $ d `howOld` unsafePerformIO todayDay

makeKokuhoMark :: Kumiai -> Text
makeKokuhoMark k =
  case (k ^. #kokuhoGet, k ^. #kokuhoLost) of
    (_, Just _)        -> "未"
    (Nothing, Nothing) -> "未"
    _                  -> ""

makeExplanation :: Kumiai -> Text
makeExplanation k = case (makeKokuhoMark k, k ^. #relational) of
                      ("未", Just r)  -> "未・" <> r
                      (""  , Just r)  -> r
                      ("未", Nothing) -> "未"
                      _   -> ""

maybeString :: Show a => Maybe a -> Text
maybeString (Just a) = pack $ show a
maybeString Nothing  = mempty
--------------------------------------------------
csvSource :: MonadIO m =>
  -- (Config -> FilePath) -> UnderConfigT m (Source IO [Text])
  (Config -> FilePath) -> UnderConfigT m (ConduitT () [Text] IO ())
csvSource f = do
  csvname <- f <$> ask
  return $ relationSpec `parseCSVSource` csvname

regularN :: Text -> Text
regularN = Tx.justifyRight 7 '0'

test = do
  conf    <- readConfig "app/Config.yaml"
  csvName <- dataCSVFileName <$> ask `runReaderT` conf
  I.hSetEncoding I.stdout I.utf8

  parseCSVSource spec csvName
    $=& CL.map makeKumiai
    $$& CL.mapM_ ((^. #rawAddress) >>> Tx.putStrLn)

kNumberMap :: UnderConfigT IO KNumberMap
kNumberMap = do
  csvName <- dataCSVFileName <$> ask

  gen <- lift $
         parseCSVSource spec csvName
         $=& CL.map makeKumiai
         $=& CL.map ((regularN . (^. #number)) &&& id)
         $$& CL.consume
  return $ M.fromList gen
---addRelational----------------------------------
relationalMap :: UnderConfigT IO OyakataMap
relationalMap = do
  source <- csvSource relationalFileName
  gen    <- lift $ source
            $=& CL.map (\[num, oyakata, oyakataN] ->
                         (regularN num, (oyakata, regularN oyakataN)))
            $$& CL.consume
  return $ M.fromList gen

relationalNotAliveCheck ::
  MonadIO m => KNumberMap -> UnderConfigT m [ErrorType]
relationalNotAliveCheck kmap = do
  source <- csvSource relationalFileName
  xl <- liftIO $ source
        $=& CL.map (\[n, _, _] -> ((`M.lookup` kmap) &&& id) $ regularN n)
        $$& CL.consume
  let xlWithLineNumber = zip [2..] xl
  liftIO $ CL.sourceList xlWithLineNumber
    $=& CL.filter (isNothing . fst . snd)
    $=& CL.map (\(ln, (_, k)) -> ChildNotFound k ln)
    $$& CL.consume

addRelationaltoKumiai :: OyakataMap -> Kumiai -> Kumiai
addRelationaltoKumiai m k =
  let key = regularN $ (k ^. #number) in
  case key `M.lookup` m of
    Just (o, _)  -> k & #relational .~ Just o
    Nothing      -> k

-- addRelationConduit :: OyakataMap -> Conduit Kumiai IO Kumiai
addRelationConduit :: OyakataMap -> ConduitT Kumiai Kumiai IO ()
addRelationConduit om =
  awaitForever (yield . addRelationaltoKumiai om)
---workReplace------------------------------------
workReplaceMap :: UnderConfig WorkMap
workReplaceMap = do
  repAlist <- workReplaceAlist <$> ask
  return $
    M.fromList $
      map (\[long, short] -> (long, short)) repAlist

workReplace :: WorkMap -> Text -> Text
workReplace m w = w `fromMaybe` (w `M.lookup` m)
---repairKumiai-----------------------------------
repairAddress :: [Text] -> Text -> Text
repairAddress targets ad =
  foldl (flip B.killStringsAll) ad targets

repairKumiai :: Kumiai -> UnderConfig Kumiai
repairKumiai k = do
  removeAddress <- addressRemove <$> ask
  repMap        <- workReplaceMap

  let k1 = k  & #work .~ (workReplace repMap (k ^. #work))
  let k2 = k1 & #address .~ (repairAddress removeAddress (k ^. #address))
  return k2
--------------------------------------------------
errorPrinterCore :: MonadIO m => Text -> m ()
errorPrinterCore = liftIO . Tx.hPutStrLn I.stderr

errorPrinter :: Text -> Source IO Text -> IO ()
errorPrinter message errors = do
  errorsList <- runConduit $ errors .| CL.consume
  when (not (null errorsList)) $ do
    errorPrinterCore message
    runConduit $ CL.sourceList errorsList .| errorSink

errorSink :: Sink Text IO ()
errorSink = awaitForever errorPrinterCore

errorSource :: Log -> Source IO ErrorType
errorSource = CL.sourceList

childNotFoundConduit :: Conduit ErrorType IO Text
childNotFoundConduit =
  awaitForever $ \e ->
    case e of
      ChildNotFound num linenum -> do
        let ln = Tx.pack $ show linenum
        yield [heredoc|${ln}行目, 組合員番号 ${num}|]
      _ -> return ()

oyakataNotFoundConduit :: Conduit ErrorType IO Text
oyakataNotFoundConduit = do
  awaitForever $ \e -> do
    case e of
      OyakataNotFound k oN -> do
        let oNT  = [heredoc|指定されている親方番号： ${oN}|]
        let knum = runK k ^. #number
        let knam = runK k ^. #name
        let num  = [heredoc|組合員番号： ${knum}|]
        let name = [heredoc|組合員氏名： ${knam}|]
        yield $ Tx.intercalate ", " [num, name, oNT]
      _ -> return ()

revOrderConduit :: Conduit ErrorType IO Text
revOrderConduit =
  awaitForever $ \e ->
    case e of
      RevOrder k oN -> do
        let oNT  = [heredoc|指定されている親方番号： ${oN}|]
        let knum = runK k ^. #number
        let knam = runK k ^. #name
        let num  = [heredoc|組合員番号： ${knum}|]
        let name = [heredoc|組合員氏名： ${knam}|]
        yield $ Tx.intercalate ", " [num, name, oNT]
      _ -> return ()

hanUnmatchConduit :: Conduit ErrorType IO Text
hanUnmatchConduit =
  awaitForever $ \e ->
    case e of
      OyakataHanUnMatch k o -> do
        let node = runK k
        let oyak = runK o
        let kB = Tx.unpack . (^. #bunkai)
        let kH = Tx.unpack . (^. #han)
        let kN = Tx.unpack . (^. #name)
        let k' = [heredoc|組合員：${kN node}(${kB node}分会・${kH node}班)|]
        let o' = [heredoc|親方：${kN oyak}(${kB oyak}分会・${kH oyak}班)|]
        yield $ Tx.pack $ intercalate ", " [k', o']
      _ -> return ()


sortConsumerPrinter :: Kumiai -> UnderConfigT (Sink Kumiai IO) ()
sortConsumerPrinter kumiai = do
  pOrder <- printOrder <$> ask
  let funcs = map translate pOrder
  (Tx.intercalate "," >>> Tx.putStrLn >>> liftIO) $
    map ($ kumiai) funcs

sortConsumerPrintOut :: [Kumiai] -> [Kumiai] -> UnderConfigT (Sink Kumiai IO) ()
sortConsumerPrintOut xl sorted = do
  let printer = sortConsumerPrinter
  if length xl == length sorted
    then forM_ sorted printer
    else forM_ xl $ \kumiai ->
           when (kumiai `notElem` sorted)
             $ printer kumiai

sortConsumer :: OyakataMap -> KNumberMap
  ->　UnderConfigT (Sink Kumiai IO) ()
sortConsumer rmap kmap = do
  bool   <- sortCR <$> ask
  xl     <- lift CL.consume
  plog   <- relationalNotAliveCheck kmap

  if bool
    then do let (sorted, log) = runWriter $ sortCRF rmap kmap xl
            sortConsumerPrintOut xl sorted

            let errors = errorSource $ log ++ plog
            liftIO $ do
              errorPrinter
                "◎ 以下の組合員は指定されている親方よりも先に名簿に現れています。"
                (errors .| revOrderConduit)
              errorPrinter
                "◎ 以下の組合員はすでに脱退しているので,「付名簿」から削除可能です。"
                (errors .| childNotFoundConduit)
              errorPrinter
                "◎ 以下の組合員について指定されている親方番号が不明です。"
                (errors .| oyakataNotFoundConduit)
              errorPrinter
                "◎ 以下の組合員と、指定されている親方とが分会または班が異なるため、無視します。"
                (errors .| hanUnmatchConduit)
              -- errors .| CL.mapM_ print
    else forM_ xl sortConsumerPrinter
---Sink Parts-------------------------------------
ukyoSink :: UnderConfigT (Sink Kumiai IO) ()
ukyoSink = do
  pOrder <- printOrder <$> ask
  let funcs = map translate pOrder

  (lift . awaitForever) $ \kumiai ->
    (Tx.intercalate "," >>> Tx.putStrLn >>> liftIO)
      $ map ($ kumiai) funcs

figureSink :: OyakataMap -> KNumberMap -> Sink Kumiai IO ()
figureSink om km = do
  xl <- CL.consume
  liftIO $ print $ runWriterT $ listToRT om km xl
---Main-------------------------------------------
(<#>)  = runReader
(<##>) = runReaderT

main :: IO ()
main = do
  opt <- Q.customExecParser (Q.prefs Q.showHelpOnError) myParserInfo

  case yamlFile opt of
    "" -> throwM $
            FileNotExistException "Set configuration-file.(e.g. -y yamlfile)"
    y  -> do
      conf <- readConfig y
      -- conf <- readConfig "app/config.yaml"
      rmap <- relationalMap <##> conf
      kmap <- kNumberMap <##> conf
      let dataName = (dataCSVFileName <$> ask) <#> conf

      encoding <- I.mkTextEncoding "cp932"
      I.hSetEncoding I.stdout encoding
      I.hSetEncoding I.stderr encoding

      if makeMapC opt
        then makeJavascriptFileKumiai ((makeMapCR <$> ask) <#> conf)
        else do let source = parseCSVSource spec dataName
                             $=& CL.map makeKumiai
                             $=& addRelationConduit rmap
                             $=& CL.map (\k -> repairKumiai k <#> conf)

                if figure opt
                  then  source $$& figureSink rmap kmap
                  else  source $$& (sortConsumer rmap kmap <##> conf)
--------------------------------------------------
data Options = Options { yamlFile :: String
                       , figure   :: Bool
                       , makeMapC :: Bool } deriving (Show)

yamlFileP :: Q.Parser String
yamlFileP = Q.strOption $ mconcat
            [ Q.short 'y', Q.long "yaml"
            , Q.help ""
            , Q.metavar ""
            , Q.value ""
            , Q.showDefaultWith id ]

figureP :: Q.Parser Bool
figureP = Q.switch $ Q.short 'f' <> Q.long "figure"    <> Q.help ""

makeMapP :: Q.Parser Bool
makeMapP = Q.switch $ Q.short 'm' <> Q.long "map"    <> Q.help ""

myParserInfo :: Q.ParserInfo Ukyo.Options
myParserInfo = Q.info optionsP $ mconcat
    [ Q.fullDesc
    , Q.progDesc "test program."
    , Q.header "snews.exe -- get a daily news article program."
    , Q.footer ""
    , Q.progDesc ""
    ]

optionsP :: Q.Parser Ukyo.Options
optionsP = (<*>) Q.helper
           $ Options
           <$> yamlFileP
           <*> figureP
           <*> makeMapP
