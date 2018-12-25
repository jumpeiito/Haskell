{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
module Match.Geocoder
  (makeJavascriptFile, Bunkai, Han, MakeMap (..))
where

import           Control.Arrow              ((>>>))
import           Control.Concurrent         (threadDelay)
import           Control.Lens               ((^.))
import           Control.Monad              (when, foldM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Control.Monad.Trans.Cont   (ContT (..), runContT)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Aeson                 (FromJSON)
import           Data.Conduit               (runConduit, (.|))
import qualified Data.Conduit.List          as CL
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as Tx
import           Data.Default.Class         (def)
import           Data.Extensible
import           Database.SQLite.Simple
import           GHC.Generics               (Generic)
import           Match.CSV                  (Spec, parseCSVSource)
import           Match.Kumiai
import           Network.HTTP.Req
import           System.Directory           (doesFileExist, removeFile)
import qualified System.IO                  as I
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Heredoc
import           Text.Heterocephalus        (compileTextFile)
import           Text.Printf                (printf)
import           Text.XML                   (Node (..), Document, parseLBS)
import           Text.XML.Cursor            ( node, fromDocument, ($//)
                                            , descendant, checkName)
import           Util.Address               (makeTypeAddress)

data MakeMap = M { mapExecute :: Bool
                 , doFetch    :: Bool
                 , mapBunkai  :: Maybe Int
                 , mapHan     :: Maybe Int }
  deriving (Show, Read, Generic)

instance FromJSON MakeMap

newtype Bunkai = Bunkai { runBunkai :: Maybe Int }
newtype Han    = Han    { runHan :: Maybe Int }

class Filtering a where
  solver   :: a -> Maybe Int
  toFilter :: a -> (Text -> Bool)

  toFilter b = case solver b of
                 Nothing -> const True
                 Just i  -> (== Tx.pack (printf "%02d" i))

instance Filtering Bunkai where solver = runBunkai
instance Filtering Han    where solver = runHan

fromConfig f = (^. f) <$> ask `runReaderT` config

data Point = Point Text Double Double deriving (Show)

pointAddress :: Point -> Text
pointLat, pointLng :: Point -> Double
pointAddress (Point t _ _) = t
pointLat (Point _ t _)     = t
pointLng (Point _ _ t)     = t

instance FromRow Point where
  fromRow = Point <$> field <*> field <*> field

instance ToRow Point where
  toRow (Point t d1 d2) = toRow (t, d1, d2)

runC :: Monad m => ContT a m a -> m a
runC = (`runContT` return)

withDB :: (FilePath -> IO a) -> IO a
withDB = (=<< fromConfig #dbname)

withDBAction :: (Connection -> IO a) -> IO a
withDBAction f = withDB (flip withConnection f)

makeNewDB :: IO ()
makeNewDB = do
  runC $ do
    db <- ContT withDB
    ex <- liftIO $ doesFileExist db
    when (not ex) $ do
      conn <- ContT $ withConnection db
      (liftIO $ execute_ conn
       "CREATE TABLE test (address Text, lat Double, lng Double)")

refreshDB :: IO ()
refreshDB = do
  withDB $ \db -> do
    ex <- doesFileExist db
    when ex $ do
      removeFile db
      makeNewDB

insertDB :: Maybe Point -> IO ()
insertDB Nothing  = return ()
insertDB (Just p@(Point address _ _)) = do
  withDBAction $ \conn -> do
    isRegistered <- lookupDB address
    if not (null isRegistered)
      then putStrLn "already registered."
      else execute conn
            "INSERT INTO test (address, lat, lng) VALUES (?,?,?)" p

lookupDB :: Text -> IO [Point]
lookupDB tx = do
  runC $ do
    conn <- ContT withDBAction
    liftIO $
      queryNamed conn "SELECT * from test where address = :a" [":a" := tx]

withLookupDB :: Kumiai -> (Kumiai -> MaybeT IO Point) -> MaybeT IO Point
withLookupDB k f = do
  dbReply <- liftIO $ lookupDB (k ^. #rawAddress)
  if (null dbReply)
    then f k
    else return $ head dbReply

allQueryDB :: IO [Point]
allQueryDB = withDBAction (flip query_ "SELECT * from test")

type Config = Record
  '[ "topURL"      >: Url 'Https
   , "dbname"      >: FilePath
   , "testcsv"     >: FilePath
   , "spec"        >: Spec
   , "jsfile"      >: FilePath
   , "waitSeconds" >: Int
   ]

config :: Config
config =    #topURL  @= https "www.geocoding.jp" /: "api"
         <: #dbname  @= "geocoder.db"
         <: #testcsv @= "組合員データ活用.csv"
         <: #spec    @= [ "支部コード"
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
         <: #jsfile @= "index.js"
         <: #waitSeconds @= 10
         <: nil

errP :: MonadIO m => String -> m ()
errP = liftIO . I.hPutStrLn I.stderr

contentsOf :: Node -> Text
contentsOf (NodeContent x) = x
contentsOf _               = mempty

getRequest :: Text -> Req LbsResponse
getRequest address = do
  url <- fromConfig #topURL
  req GET url NoReqBody lbsResponse ("q" =: address)

parseXML :: Document -> [Double]
parseXML x =
  let nameP n = (n == "lat") || (n == "lng")
  in let child'  = fromDocument x $// checkName nameP
  in let parser  = node >>> contentsOf >>> unpack >>> read
  in map parser (concat $ map descendant child')

getLatLng :: Text -> IO [Double]
getLatLng address = do
  runReq def $ do
    r <- getRequest address
    case parseLBS def (responseBody r) of
      Right x -> return $ parseXML x
      Left _  -> return []

retryGetPoint :: Text -> MaybeT IO Point
retryGetPoint address = do
  w <- fromConfig #waitSeconds
  errP [heredoc|retry after ${show w} seconds.|]
  liftIO $ threadDelay (w * 1000 * 1000)
  -- getPointして取得できなかった場合は,return Nothingする。
  Point _ la ln <- getPoint $ makeTypeAddress address ^. #town
  errP [heredoc|success to retry|]
  return $ Point address la ln

getPoint :: Text -> MaybeT IO Point
getPoint address = do
  latlng <- liftIO $ getLatLng address
  case latlng of
    [lat', lng'] -> return $ Point address lat' lng'
    -- 取得できなかった場合は、住所の中の集合住宅部分を削除して再度getPointを試
    -- みる。
    _ -> do
      -- 集合住宅部分を削除した結果、
      if makeTypeAddress address ^. #town == address
        -- 元の住所と同じであった場合(繰り返しの基底条件)…何もしない
        then MaybeT (return Nothing)
        -- 異なる場合…集合住宅部分を削除したもので再取得を試みる。
        else retryGetPoint address

insertPoint :: Kumiai -> MaybeT IO Point
insertPoint k = MaybeT $ do
  let address' = k ^. #rawAddress
  errP [heredoc|Getting geocode, ${Tx.unpack address'}|]
  point' <- runMaybeT $ getPoint address'
  case point' of
    Nothing -> errP [heredoc|failed to get geocode, ${Tx.unpack address'}|]
    Just p  -> insertDB $ Just p
  waitS <- fromConfig #waitSeconds
  errP [heredoc|${show waitS} seconds wait.|]
  threadDelay (waitS * 1000 * 1000)
  return point'

fetch, onlyGet :: Kumiai -> MaybeT IO Point
fetch k   = withLookupDB k insertPoint
onlyGet k = withLookupDB k (const (MaybeT (return Nothing)))

makeKumiaiTable :: Bunkai -> Han -> IO [Kumiai]
makeKumiaiTable b h = do
  csv  <- fromConfig #testcsv
  spec <- fromConfig #spec
  runConduit
    $ parseCSVSource spec csv
    .| CL.map makeKumiai
    .| CL.filter ((^. #bunkaiCode) >>> (toFilter b))
    .| CL.filter ((^. #han) >>> (toFilter h))
    .| CL.consume

errorAtGet :: Kumiai -> IO ()
errorAtGet k = do
  let kumiaiName = Tx.unpack $ k ^. #name
  putStrLn [heredoc|${kumiaiName}の地図情報を入手できませんでした。|]

errorAtDB :: Kumiai -> IO ()
errorAtDB k = do
  let b = Tx.unpack $ k ^. #bunkai
  let h = Tx.unpack $ k ^. #han
  let n = Tx.unpack $ k ^. #name
  putStrLn [heredoc|${b}分会 ${h}班 ${n}の地図情報がありません。|]

mapTargetInsert :: Bool -> [PointK] -> Kumiai -> IO [PointK]
mapTargetInsert doFetchP pk k = do
  let (getF, errorF) = if doFetchP
                          then (fetch, errorAtGet)
                          else (onlyGet, errorAtDB)
  f <- runMaybeT $ getF k
  case f of
    Just p  -> return $ makePointK p k : pk
    Nothing -> do
      errorF k
      return pk

mapTargets :: Bool -> Bunkai -> Han -> IO [PointK]
mapTargets doFetchP b h = do
  kt <- makeKumiaiTable b h
  foldM (mapTargetInsert doFetchP) [] kt

data PointK = PK { lat    :: Double
                 , lng    :: Double
                 , point  :: Point
                 , name   :: Text
                 , bunkai :: Text
                 , han    :: Text } deriving (Show)

makePointK :: Point -> Kumiai -> PointK
makePointK p k = PK { lat    = pointLat p
                    , lng    = pointLng p
                    , point  = p
                    , name   = k ^. #name
                    , bunkai = k ^. #bunkai
                    , han    = k ^. #han }

makeJavascript :: Bool -> Bunkai -> Han -> IO String
makeJavascript doFetchP b h = do
  mt <- mapTargets doFetchP b h
  return $ renderMarkup $
    $(compileTextFile "src/Match/templateJS.txt")

javaScriptOutputToFile :: MonadIO m => String -> I.Handle -> m ()
javaScriptOutputToFile jscript h = do
  encoding <- liftIO $ I.mkTextEncoding "cp932"
  liftIO $ I.hSetEncoding h encoding
  liftIO $ I.hPutStrLn h jscript

makeJavascriptFile :: MakeMap -> IO ()
makeJavascriptFile mm = do
  case mapExecute mm of
    False -> return ()
    True  -> do
      let b = Bunkai (mapBunkai mm)
      let h = Han (mapHan mm)
      let f = doFetch mm
      js   <- makeJavascript f b h
      file <- fromConfig #jsfile
      runC $ do
        handle <- ContT $ I.withFile file I.WriteMode
        javaScriptOutputToFile js handle
