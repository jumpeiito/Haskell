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
--- data definitions -----------------------------
data Point = Point Text Double Double deriving (Show)

instance FromRow Point where
  fromRow = Point <$> field <*> field <*> field

instance ToRow Point where
  toRow (Point t d1 d2) = toRow (t, d1, d2)

type JSUnit = Record
  '[ "latitude"  >: Double
   , "longitude" >: Double
   , "label"     >: Label]

type Label = Record
  '[ "address"     >: Text
   , "explanation" >: Text]

type Config = Record
  '[ "topURLhost"  >: Text
   , "topURLrest"  >: Text
   , "dbname"      >: FilePath
   , "testcsv"     >: FilePath
   , "spec"        >: Spec
   , "jsfile"      >: FilePath
   , "waitSeconds" >: Int ]

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
--- Utilities ------------------------------------
conf :: IO Config
conf = readYaml "src/mapConfig.yaml"

setting :: MonadIO m => Getting b Config b -> m b
setting f = liftIO $ do
  c <- conf
  ((^. f) <$> ask) `runReaderT` c

runC :: Monad m => ContT a m a -> m a
runC = (`runContT` return)

errP :: MonadIO m => String -> m ()
errP = liftIO . I.hPutStrLn I.stderr
--- Database Manupilation ------------------------
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

withLookupDB :: Label -> (Label -> MaybeT IO Point)
  -> MaybeT IO Point
withLookupDB l f = do
  dbReply <- liftIO $ lookupDB (l ^. #address)
  if (null dbReply)
    then f l
    else return $ head dbReply

allQueryDB :: IO [Point]
allQueryDB = withDBAction (flip query_ "SELECT * from test")
--- Throw query and parse ------------------------
contentsOf :: Node -> Text
contentsOf (NodeContent x) = x
contentsOf _               = mempty

getRequest :: Text -> Req LbsResponse
getRequest address = do
  host <- setting #topURLhost
  rest <- setting #topURLrest
  let url = https host /: rest
  req GET url NoReqBody lbsResponse ("q" =: address)

parseXML :: Document -> [Double]
parseXML x =
  let nameP n   = (n == "lat") || (n == "lng")
  in let child' = fromDocument x $// checkName nameP
  in let parser = node >>> contentsOf >>> unpack >>> read
  in map parser (concat $ map descendant child')

getLatLng :: Text -> IO [Double]
getLatLng address = do
  runReq def $ do
    r <- getRequest address
    case parseLBS def (responseBody r) of
      Right x -> return $ parseXML x
      Left _  -> return mempty

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
--- For Kumiai's sake ----------------------------
makeLabel :: Kumiai -> Label
makeLabel k =
  let doc = Tx.intercalate "/"
            [ (k ^. #bunkai) <> "分会"
            , (k ^. #han) <> "班"
            , k ^. #name]
  in let ad = k ^. #rawAddress
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

insertPoint :: Label -> MaybeT IO Point
insertPoint label = MaybeT $ do
  let e = label `fromLabelText` #explanation
  let a = label `fromLabelText` #address
  errP [heredoc|Getting geocode, ${e} ${a}|]
  point' <- runMaybeT $ getPoint $ label ^. #address
  case point' of
    Nothing -> do
      errP [heredoc|failed to get geocode, ${e} ${a}|]
    Just p  -> insertDB $ Just p
  waitS <- setting #waitSeconds
  errP [heredoc|${show waitS} seconds wait.|]
  threadDelay (waitS * 1000 * 1000)
  return point'

toString :: Label -> Text
toString l = l ^. #explanation <> "/" <> l ^. #address

fromLabelText :: s -> Getting Text s Text -> String
fromLabelText l sym = Tx.unpack $ l ^. sym

fetch, onlyGet :: Label -> MaybeT IO Point
fetch k   = withLookupDB k insertPoint
onlyGet k = withLookupDB k (const (MaybeT (return Nothing)))

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

mapTargets :: Bool -> [Label] -> IO [JSUnit]
mapTargets doFetchP labels = do
  foldM (mapTargetInsert doFetchP) [] labels

makeJavascript :: [JSUnit] -> IO String
makeJavascript jsunits = do
  let contents = map (\j -> ( j ^. #latitude
                            , j ^. #longitude
                            , toString (j ^. #label))) jsunits
  return $ renderMarkup $
    $(compileTextFile "src/templateJS2.txt")

javascriptOutputToFile :: MonadIO m => String -> I.Handle -> m ()
javascriptOutputToFile jscript h = liftIO $ do
  I.hSetEncoding h I.utf8
  I.hPutStrLn h jscript

makeJavascriptFileContents :: Bool -> [Label] -> IO ()
makeJavascriptFileContents doFetchP labels = do
  cp   <- mapTargets doFetchP labels
  js   <- makeJavascript cp
  file <- setting #jsfile
  runC $ do
    handle <- ContT $ I.withFile file I.WriteMode
    javascriptOutputToFile js handle

makeJavascriptFileKumiai :: MakeMap -> IO ()
makeJavascriptFileKumiai mm = do
  guard $ mapExecute mm
  let b = mapBunkai mm
  let h = mapHan mm
  c <- makeKumiaiTable (Bunkai b) (Han h)
  makeJavascriptFileContents (doFetch mm) c

labelListFromCSV :: FilePath -> IO [Label]
labelListFromCSV fp = do
  let spec   = ["住所", "備考"]
  runConduit
    $  parseCSVSource spec fp
    .| CL.map (\[ad, ex] ->
                 #address @= ad <: #explanation @= ex <: nil)
    .| CL.consume

makeJavascriptFromCSV :: Bool -> FilePath -> IO ()
makeJavascriptFromCSV doFetchP fp = do
  labels <- labelListFromCSV fp
  makeJavascriptFileContents doFetchP labels

testMM :: MakeMap
testMM = M True False Nothing Nothing
