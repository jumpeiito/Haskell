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
import           Text.XML                   (Node (..), Document)
import           Text.XML.Cursor            ( node, fromDocument, ($//)
                                            , descendant, checkName)
import           Util.Address               (makeTypeAddress, Address)

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

-- Database Type
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
withDB f = do
  db <- fromConfig #dbname
  f db

withDBAction :: (Connection -> IO a) -> IO a
withDBAction f = withDB (flip withConnection f)

makeNewDB :: IO ()
makeNewDB = do
  let executeM_ c q = liftIO $ execute_ c q
  runC $ do
    db <- ContT withDB
    ex <- liftIO $ doesFileExist db
    when (not ex) $ do
      conn <- ContT $ withConnection db
      (executeM_ conn
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
insertDB (Just p) = do
  withDBAction $ \conn -> do
    let Point address _ _ = p
    isRegistered <- lookupDB address
    case isRegistered of
      [] -> execute conn "INSERT INTO test (address, lat, lng) VALUES (?,?,?)" p
      _  -> putStrLn "already registered."

lookupDB :: Text -> IO [Point]
lookupDB tx = do
  withDBAction $ \conn ->
    queryNamed conn "SELECT * from test where address = :a" [":a" := tx]

allQueryDB :: IO [Point]
allQueryDB = do
  withDBAction (flip query_ "SELECT * from test")

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

errP :: String -> IO ()
errP = I.hPutStrLn I.stderr

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

retryGetPoint :: Text -> IO (Maybe Point)
retryGetPoint address = do
  w <- fromConfig #waitSeconds
  errP [heredoc|retry after ${show w} seconds.|]
  threadDelay (w * 1000 * 1000)
  let p = makeTypeAddress address
  town <- getPoint $ p ^. #town
  case town of
    Nothing              -> return Nothing
    Just (Point _ la ln) -> do
      errP [heredoc|success to retry|]
      return $ Just $ Point address la ln

getPoint :: Text -> IO (Maybe Point)
getPoint address = do
  latlng <- getLatLng address
  case latlng of
    [lat', lng'] -> return $ Just $ Point address lat' lng'
    _ -> do
      let p = makeTypeAddress address
      if p ^. #town == address
        then return Nothing
        else retryGetPoint address

insertPoint :: Kumiai -> IO (Maybe Point)
insertPoint k = do
  let address' = k ^. #rawAddress
  errP [heredoc|Getting geocode, ${Tx.unpack address'}|]
  point' <- getPoint address'
  case point' of
    Nothing -> do
      errP [heredoc|failed to get geocode, ${Tx.unpack address'}|]
    Just p  -> insertDB $ Just p
  waitS <- fromConfig #waitSeconds
  errP [heredoc|${show waitS} seconds wait.|]
  threadDelay (waitS * 1000 * 1000)
  return point'

withLookupDB :: Kumiai -> (Kumiai -> IO (Maybe Point)) -> IO (Maybe Point)
withLookupDB k f = do
  dbReply <- lookupDB (k ^. #rawAddress)
  if (null dbReply)
    then f k
    else return $ Just $ head dbReply

fetch, onlyGet :: Kumiai -> IO (Maybe Point)
fetch k   = withLookupDB k insertPoint
onlyGet k = withLookupDB k (return . (const Nothing))

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
  f <- getF k
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
