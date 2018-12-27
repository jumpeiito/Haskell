{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies     #-}
module Match.Geocoder
  (makeJavascriptFileKumiai
   , makeJavascriptFromCSV
   , makeJavascriptFileContents
   ,Bunkai, Han, MakeMap (..))
where

import           Control.Arrow              ((>>>))
import           Control.Concurrent         (threadDelay)
import           Control.Lens               ((^.))
import           Control.Lens.Getter        (Getting)
import           Control.Monad              (when, foldM, guard)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ask, runReaderT, runReader)
import           Control.Monad.Trans.Cont   (ContT (..), runContT)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Aeson                 (FromJSON)
import           Data.Conduit               (runConduit, (.|))
import qualified Data.Conduit.List          as CL
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as Tx
import           Data.Default.Class         (def)
import           Data.Extensible
import           Data.Monoid                ((<>))
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
import           Util.Yaml

setting :: MonadIO m => Getting b Config b -> m b
setting f = liftIO $ do
  c <- conf
  ((^. f) <$> ask) `runReaderT` c

data Point = Point Text Double Double deriving (Show)

pointAddress       :: Point -> Text
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
withDB f = setting #dbname >>= f

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

allQueryDB :: IO [Point]
allQueryDB = withDBAction (flip query_ "SELECT * from test")

type Config = Record
  '[ "topURLhost"  >: Text
   , "topURLrest"  >: Text
   , "dbname"      >: FilePath
   , "testcsv"     >: FilePath
   , "spec"        >: Spec
   , "jsfile"      >: FilePath
   , "waitSeconds" >: Int
   ]

conf :: IO Config
conf = readYaml "src/mapConfig.yaml"

errP :: MonadIO m => String -> m ()
errP = liftIO . I.hPutStrLn I.stderr

contentsOf :: Node -> Text
contentsOf (NodeContent x) = x
contentsOf _               = mempty

getRequest :: Text -> IO (Req LbsResponse)
getRequest address = do
  c <- conf
  host <- setting #topURLhost
  rest <- setting #topURLrest
  let url = https host /: rest
  return $ req GET url NoReqBody lbsResponse ("q" =: address)

parseXML :: Document -> [Double]
parseXML x =
  let nameP n   = (n == "lat") || (n == "lng")
  in let child' = fromDocument x $// checkName nameP
  in let parser = node >>> contentsOf >>> unpack >>> read
  in map parser (concat $ map descendant child')

getLatLng :: Text -> IO [Double]
getLatLng address = do
  ioaddress <- getRequest address
  runReq def $ do
    r <- ioaddress
    case parseLBS def (responseBody r) of
      Right x -> return $ parseXML x
      Left _  -> return []

retryGetPoint :: Text -> MaybeT IO Point
retryGetPoint address = do
  w <- setting #waitSeconds
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
--------------------------------------------------
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

makeLabel :: Kumiai -> Label
makeLabel k =
  let exp = [ (k ^. #bunkai) <> "分会"
            , (k ^. #han) <> "班"
            , k ^. #name]
  in let ad = k ^. #rawAddress
  in let doc = Tx.intercalate "/" exp
  in    #address @= ad
     <: #explanation @= doc
     <: nil

makeKumiaiTable :: Bunkai -> Han -> IO [Label]
makeKumiaiTable b h = do
  csv  <- setting #testcsv
  spec <- setting #spec
  runConduit
    $ parseCSVSource spec csv
    .| CL.map makeKumiai
    .| CL.filter ((^. #bunkaiCode) >>> (toFilter b))
    .| CL.filter ((^. #han) >>> (toFilter h))
    .| CL.map makeLabel
    .| CL.consume

javascriptOutputToFile :: MonadIO m => String -> I.Handle -> m ()
javascriptOutputToFile jscript h = liftIO $ do
  encoding <- I.mkTextEncoding "cp932"
  I.hSetEncoding h encoding
  I.hPutStrLn h jscript

withLookupDB :: Label -> (Label -> MaybeT IO Point)
  -> MaybeT IO Point
withLookupDB l f = do
  let a = l ^. #address
  dbReply <- liftIO $ lookupDB a
  if (null dbReply)
    then f l
    else return $ head dbReply

errorAtGet :: Label -> IO ()
errorAtGet label = do
  let pre = label `fromLabelText` #explanation
  let ad  = label `fromLabelText` #address
  putStrLn
    [heredoc|${pre} ${ad}の地図情報を入手できませんでした。|]

errorAtDB :: Label -> IO ()
errorAtDB label = do
  let pre = label `fromLabelText` #explanation
  let ad  = label `fromLabelText` #address
  putStrLn
    [heredoc|${pre} ${ad}の地図情報がありません。|]

mapTargetInsert :: Bool -> [JSUnit]
  -> Label -> IO [JSUnit]
mapTargetInsert doFetchP pk label = do
  let (getF, errorF) = if doFetchP
                          then (fetch, errorAtGet)
                          else (onlyGet, errorAtDB)
  f <- runMaybeT $ getF label
  case f of
    Just (Point _ la ln)  -> do
      let jsu =  #latitude  @= la
              <: #longitude @= ln
              <: #label     @= label
              <: nil
      return $ jsu : pk
    Nothing -> do
      errorF label
      return pk

insertPoint :: Label -> MaybeT IO Point
insertPoint label = MaybeT $ do
  let p = label `fromLabelText` #explanation
  let a = label `fromLabelText` #address
  errP [heredoc|Getting geocode, ${p} ${a}|]
  point' <- runMaybeT $ getPoint $ label ^. #address
  case point' of
    Nothing -> do
      errP [heredoc|failed to get geocode, ${p} ${a}|]
    Just p  -> insertDB $ Just p
  waitS <- setting #waitSeconds
  errP [heredoc|${show waitS} seconds wait.|]
  threadDelay (waitS * 1000 * 1000)
  return point'

fetch, onlyGet :: Label -> MaybeT IO Point
fetch k   = withLookupDB k insertPoint
onlyGet k = withLookupDB k (const (MaybeT (return Nothing)))

mapTargets :: Bool -> [Label] -> IO [JSUnit]
mapTargets doFetch labels = do
  foldM (mapTargetInsert doFetch) [] labels

makeJavascript :: [JSUnit] -> IO String
makeJavascript jsunits = do
  let contents = map (\j -> ( j ^. #latitude
                            , j ^. #longitude
                            , toString (j ^. #label))) jsunits
  return $ renderMarkup $
    $(compileTextFile "src/templateJS2.txt")

makeJavascriptFileContents :: Bool -> [Label] -> IO ()
makeJavascriptFileContents doFetchP labels = do
  cp   <- mapTargets doFetchP labels
  js   <- makeJavascript cp
  file <- setting #jsfile
  runC $ do
    handle <- ContT $ I.withFile file I.WriteMode
    javascriptOutputToFile js handle

type Label = Record
  '[ "address"     >: Text
   , "explanation" >: Text]

toString :: Label -> Text
toString l = l ^. #explanation <> "/" <> l ^. #address

fromLabelText l sym = Tx.unpack $ l ^. sym

type JSUnit = Record
  '[ "latitude"  >: Double
   , "longitude" >: Double
   , "label"     >: Label]

labelListFromCSV :: FilePath -> IO [Label]
labelListFromCSV fp = do
  let spec   = ["住所", "備考"]
  runConduit
    $  parseCSVSource spec fp
    .| CL.map (\[ad, ex] ->
                 #address @= ad <: #explanation @= ex <: nil)
    .| CL.consume

makeJavascriptFromCSV :: Bool -> FilePath -> IO ()
makeJavascriptFromCSV doFetch fp = do
  labels <- labelListFromCSV fp
  makeJavascriptFileContents doFetch labels

makeJavascriptFileKumiai :: MakeMap -> IO ()
makeJavascriptFileKumiai mm = do
  guard $ mapExecute mm
  let b = mapBunkai mm
  let h = mapHan mm
  c <- makeKumiaiTable (Bunkai b) (Han h)
  makeJavascriptFileContents (doFetch mm) c

testMM = M True False Nothing Nothing
