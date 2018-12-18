{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Ukyo.Main where
-- module Main where

import           Control.Arrow             ((>>>), (&&&))
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
import qualified Data.Conduit.List         as CL
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
import qualified Options.Applicative       as Q
import qualified System.IO                 as I
import           System.IO.Unsafe          (unsafePerformIO)
import           Util.Exception            (FileNotExistException (..))
import           Util.Strdt                (todayDay, howOld)
import           Util.Yaml                 (readYaml)

-- $setup
-- >>> :set -XOverloadedStrings

data Config = C { dataCSVFileName    :: FilePath
                , sortCR             :: Bool
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
    s  = kShibuCode
    b  = kBunkaiCode
    h  = kHan
    pr = (* 1000) . fromJust . kPrintOrder
    pt = Tx.justifyRight 5 '0' . Tx.takeWhile (/= '.') . Tx.pack . show . pr

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
translate ShibuCode        k = kShibuCode k
translate Shibu            k = kShibu k
translate BunkaiCode       k = kBunkaiCode k
translate Bunkai           k = kBunkai k
translate Han              k = kHan k
translate KName            k = kName k
translate KNumber          k = kNumber k
translate KKana            k = kKana k
translate KSex             k = kSex k
translate KAddress         k = kAddress k
translate KPostal          k = kPostal k
translate KPhone           k = kPhone k
translate KCellPhone       k = kCellPhone k
translate KFax             k = kFax k
translate KKind            k = kKind k
translate KKyousai         k = kKyousai k
translate KHonbuY          k = (>>->>) $ hyToString <$> kHonbuY k
translate KShibuY          k = (>>->>) $ syToString <$> kShibuY k
translate KBunkaiY         k = (>>->>) $ byToString <$> kBunkaiY k
translate KHanY            k = (>>->>) . kHanY $ k
translate KWork            k = kWork k
translate KOffice          k = kOffice k
translate KPrintOrder      k = (>>->>) $ Tx.pack . show <$> kPrintOrder k
translate KGot             k = maybeString $ kGot k
translate KLost            k = maybeString $ kLost k
translate KBirthday        k = maybeString $ kBirthday k
translate HowOld           k = makeHowOld (kBirthday k)
translate KKokuhoGet       k = maybeString $ kKokuhoGet k
translate KKokuhoLost      k = maybeString $ kKokuhoLost k
translate Owner            _ = ""
translate BunkaichoMark    k = makeBCMark $ byToString <$> kBunkaiY k
translate BunkaiKaikeiMark k = makeBKMark $ byToString <$> kBunkaiY k
translate HanchoMark       k = makeHMark $ kHanY k
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
makeMark target part ret = if target `contains` Tx.pack part
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
  case (kKokuhoGet k, kKokuhoLost k) of
    (_, Just _)        -> "未"
    (Nothing, Nothing) -> "未"
    _                  -> ""

makeExplanation :: Kumiai -> Text
makeExplanation k = case (makeKokuhoMark k, relational k) of
                      ("未", Just r)  -> "未・" <> r
                      ("", Just r)    -> r
                      ("未", Nothing) -> "未"
                      _   -> ""

maybeString :: Show a => Maybe a -> Text
maybeString (Just a) = pack $ show a
maybeString Nothing  = mempty
--------------------------------------------------
csvSource :: MonadIO m =>
  (Config -> FilePath) -> UnderConfigT m (Source IO [Text])
csvSource f = do
  csvname <- f <$> ask
  return $ relationSpec `parseCSVSource` csvname

regularN :: Text -> Text
regularN = Tx.justifyRight 7 '0'

kNumberMap :: UnderConfigT IO KNumberMap
kNumberMap = do
  csvName <- dataCSVFileName <$> ask

  let source = parseCSVSource spec csvName
  gen <- (lift . runConduit) $
         source
         .| CL.map makeKumiai
         .| CL.map ((regularN . kNumber) &&& id)
         .| CL.consume
  return $ M.fromList gen
---addRelational----------------------------------
relationalMap :: UnderConfigT IO OyakataMap
relationalMap = do
  source <- csvSource relationalFileName
  gen    <- (lift . runConduit) $
            source
            .| CL.map (\[num, oyakata, oyakataN] ->
                         (regularN num, (oyakata, regularN oyakataN)))
            .| CL.consume
  return $ M.fromList gen

relationalNotAliveCheck ::
  MonadIO m => KNumberMap -> UnderConfigT m [ErrorType]
relationalNotAliveCheck kmap = do
  source <- csvSource relationalFileName
  xl <- (liftIO . runConduit) $ source
        .| CL.map (\[n, _, _] -> ((`M.lookup` kmap) &&& id) $ regularN n)
        .| CL.consume
  let xlWithLineNumber = zip [2..] xl
  (liftIO . runConduit) $ CL.sourceList xlWithLineNumber
    .| CL.filter (isNothing . fst . snd)
    .| CL.map (\(ln, (_, k)) -> ChildNotFound k ln)
    .| CL.consume

addRelationaltoKumiai :: OyakataMap -> Kumiai -> Kumiai
addRelationaltoKumiai m k =
  let key = regularN $ kNumber k in
  case key `M.lookup` m of
    Just (o, _)  -> k { relational = Just o }
    Nothing      -> k

addRelationConduit :: OyakataMap -> Conduit Kumiai IO Kumiai
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

  return $ k { kWork    = workReplace repMap (kWork k)
             , kAddress = repairAddress removeAddress (kAddress k)}
--------------------------------------------------
errorPrinter = liftIO . Tx.hPutStrLn I.stderr

errorSource :: Log -> Source IO ErrorType
errorSource = CL.sourceList

childNotFoundConduit :: Conduit ErrorType IO Text
childNotFoundConduit =
  awaitForever $ \e ->
    case e of
      ChildNotFound num linenum -> do
        let ln = Tx.pack $ show linenum
        yield $ ln <> "行目, 組合員番号 " <> num
      _ -> return ()

oyakataNotFoundConduit :: Conduit ErrorType IO Text
oyakataNotFoundConduit =
  awaitForever $ \e ->
    case e of
      OyakataNotFound k oN -> do
        let oNT  = "指定されている親方番号： " <> oN
        let num  = "組合員番号： " <> kNumber (runK k)
        let name = "組合員氏名： " <> kName (runK k)
        yield $ Tx.intercalate ", " [num, name, oNT]
      _ -> return ()

errorSink :: Sink Text IO ()
errorSink = awaitForever (liftIO . Tx.hPutStrLn I.stderr)

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
            (liftIO . runConduit) $ do
              errorPrinter
                "◎ 以下の組合員はすでに脱退しているので,「付名簿」から削除可能です。"
              errors .| childNotFoundConduit .| errorSink
              errorPrinter
                "◎ 以下の組合員について指定されている親方番号が不明です。"
              errors .| oyakataNotFoundConduit .| errorSink
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

      let source = parseCSVSource spec dataName
                   .| CL.map makeKumiai
                   .| addRelationConduit rmap
                   .| CL.map (\k -> repairKumiai k <#> conf)

      if figure opt
        then runConduit $ source .| figureSink rmap kmap
        else runConduit $ source .| (sortConsumer rmap kmap <##> conf)
--------------------------------------------------
data Options = Options { yamlFile :: String
                       , figure   :: Bool} deriving (Show)

yamlFileP :: Q.Parser String
yamlFileP = Q.strOption $ mconcat
            [ Q.short 'y', Q.long "yaml"
            , Q.help ""
            , Q.metavar ""
            , Q.value ""
            , Q.showDefaultWith id ]

figureP :: Q.Parser Bool
figureP = Q.switch $ Q.short 'f' <> Q.long "figure"    <> Q.help ""

myParserInfo :: Q.ParserInfo Options
myParserInfo = Q.info optionsP $ mconcat
    [ Q.fullDesc
    , Q.progDesc "test program."
    , Q.header "snews.exe -- get a daily news article program."
    , Q.footer ""
    , Q.progDesc ""
    ]

optionsP :: Q.Parser Options
optionsP = (<*>) Q.helper
           $ Options
           <$> yamlFileP
           <*> figureP
