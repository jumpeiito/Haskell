{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
module Match.Geocoder
  (makeJavascriptFileRapper, Bunkai, Han, MakeMap (..))
where

import           Control.Arrow              ((>>>))
import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Monad              (when, foldM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as Tx
import           Data.Default.Class
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
import           Text.Heterocephalus
import           Text.Printf
import           Text.XML
import           Text.XML.Cursor
import           Util.Address

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

withDB :: (FilePath -> IO a) -> IO a
withDB f = do
  db <- fromConfig #dbname
  f db

withDBAction :: (Connection -> IO a) -> IO a
withDBAction f = do
  withDB $ \db -> do
    conn <- open db
    q <- f conn
    close conn
    return q

makeNewDB :: IO ()
makeNewDB = do
  withDB $ \db -> do
    ex <- doesFileExist db
    when (not ex) $ do
      conn <- open db
      execute_ conn "CREATE TABLE test (address Text, lat Double, lng Double)"
      close conn

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
  liftIO . withDBAction $ \conn -> do
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
  withDBAction $ \conn -> query_ conn "SELECT * from test"

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
         <: #dbname  @= "c:/Users/Jumpei/Haskell/Match/geocoder.db"
         <: #testcsv @= "c:/Users/Jumpei/Haskell/Match/app/組合員データ活用.csv"
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
         <: #jsfile @= "c:/Users/Jumpei/Haskell/Match/index.js"
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

getLatLng :: Text -> IO [Double]
getLatLng address = do
  runReq def $ do
    r <- getRequest address
    case parseLBS def (responseBody r) of
      Left _ -> return []
      Right x -> do
        let nameP n = (n == "lat") || (n == "lng")
        let child'  = fromDocument x $// checkName nameP
        let parser  = node >>> contentsOf >>> unpack >>> read
        return (map parser (concat $ map descendant child'))

retryGetPoint :: Text -> IO (Maybe Point)
retryGetPoint address = do
  let p = makeTypeAddress address
  w <- fromConfig #waitSeconds
  errP [heredoc|retry after ${show w} seconds.|]
  threadDelay (w * 1000 * 1000)
  p2 <- getPoint $ p ^. #town
  case p2 of
    Just (Point _ la ln) -> do
      errP [heredoc|success to retry|]
      return $ Just $ Point address la ln
    Nothing -> return Nothing

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
  waitS <- fromConfig #waitSeconds
  errP [heredoc|Getting geocode, ${Tx.unpack address'}|]
  point' <- getPoint address'
  case point' of
    Nothing -> do
      errP [heredoc|failed to get geocode, ${Tx.unpack address'}|]
    Just p  -> insertDB $ Just p
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
    $(compileTextFile "c:/Users/Jumpei/Haskell/Match/src/Match/templateJS.txt")

makeJavascriptFile :: Bool -> Bunkai -> Han -> IO ()
makeJavascriptFile doFetchP b h = do
  js   <- makeJavascript doFetchP b h
  file <- fromConfig #jsfile
  I.withFile file I.WriteMode $ \handle -> do
    encoding <- I.mkTextEncoding "cp932"
    I.hSetEncoding handle encoding
    I.hPutStrLn handle js

makeJavascriptFileRapper :: MakeMap -> IO ()
makeJavascriptFileRapper mp = do
  case mapExecute mp of
    False -> return ()
    True  -> do
      let b = Bunkai (mapBunkai mp)
      let h = Han (mapHan mp)
      let f = doFetch mp
      makeJavascriptFile f b h
