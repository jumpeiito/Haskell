{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE TypeFamilies             #-}
module Match.Directory
  (createHihoDirectory, removeBlankDirectory) where

-- import Foreign.C.Types
-- import Foreign.C.String

import           Control.Arrow              ((>>>))
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.Skeleton
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Either                (isRight, lefts, rights)
import           Data.Extensible
import qualified Data.List                  as DL
import           Data.List.Split            (splitOn)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  ( Text
                                            , isInfixOf
                                            , intercalate
                                            , unpack)
import           Data.Vector                ((!))
import qualified Data.Vector                as V
import           Match.Base                 ( killBlanks
                                            , officeTypeReplace
                                            , toCode
                                            , toShibu)
import           Match.Config               ( directorySpecF
                                            , fileTreeDirectory
                                            , sendCSVFileName)
import           Match.CSV                  (parseCSV2)
import           System.Directory           ( createDirectoryIfMissing
                                            , doesDirectoryExist
                                            , doesFileExist
                                            , getDirectoryContents
                                            , removeDirectory)
import           System.FilePath
import           System.IO                  (hFlush, stdout)
import           Text.Parsec
import           Text.Parsec.String
import           Text.Read                  (readMaybe)
import           Util                       ( listDirectory
                                            , makeListMap )

type TreeDirectoryMap = M.Map (Maybe Int) [(String, String, Bool)]
type XTDMap = M.Map (Maybe Int) [XTD]

type FP = (V.Vector FilePath, Int)

data DirectoryM x where
  Up       :: FP -> DirectoryM FP
  Down     :: FP -> DirectoryM FP
  Init     :: FP -> DirectoryM FP
  ShibuD   :: FP -> DirectoryM FP
  OfficeD  :: FP -> DirectoryM FP
  PersonD  :: FP -> DirectoryM FP
  Bottom   :: FP -> DirectoryM FP
  LostP    :: FP -> DirectoryM Bool
  Length   :: FP -> DirectoryM Int
  Enough   :: FP -> DirectoryM Bool
  Repeat   :: Int -> (FP -> DirectoryM FP) -> FP -> DirectoryM FP
  PWD      :: FP -> DirectoryM FilePath
  FullPath :: FP -> DirectoryM FilePath

type DM = Skeleton DirectoryM
type MonadFilePath = DM FP

up           = bone . Up
down         = bone . Down
initial      = bone . Init
pwd          = bone . PWD
shibud       = bone . ShibuD
officed      = bone . OfficeD
persond      = bone . PersonD
fullp        = bone . FullPath
bottom       = bone . Bottom
rep i s      = bone . (Repeat i s)
lostp        = bone . LostP
pathLength   = bone . Length
enoughLength = bone . Enough

hoge = "y:/ro/dee-Gov/62公文書(hoge)/030306_hoge/foo"
hogeM = monadicPath hoge

runDM :: DM a -> a
runDM m =
  case debone m of
    Repeat 0 f fp :>>= k ->
      runDM $ k fp
    Repeat n f fp :>>= k ->
      runDM $ (bone (f fp) >>= rep (n-1) f >>= k)
    Up (v, i) :>>= k ->
      runDM $ k $ (v, i - 1)
    Down (v, i) :>>= k   ->
      runDM $ k $ (v, i + 1)
    Init (v, _) :>>= k   ->
      runDM $ k $ (v, 0)
    PWD (v, i) :>>= k ->
      runDM $ k $ v ! (i - 1)
    Bottom (v, _) :>>= k ->
      runDM $ k $ (v, V.length v - 1)
    FullPath (v, i) :>>= k -> do
      let xlist = map (v!) [0..i-1]
      runDM $ k $ DL.intercalate "/" xlist
    Length (v, _) :>>= k ->
      runDM $ k $ V.length v
    LostP (v, _) :>>= k ->
      runDM $ k $ (V.length v == 7)
    Enough (v, _) :>>= k ->
      runDM $ k $ (V.length v >= 6)
    ShibuD ps@(_, _) :>>= k ->
      runDM (initial ps >>= rep 4 Down >>= k)
    OfficeD ps@(_, _) :>>= k ->
      runDM (initial ps >>= rep 5 Down >>= k)
    PersonD ps@(_, _) :>>= k ->
      runDM (initial ps >>= rep 6 Down >>= k)
    Return a -> a

shibuCodeM :: MonadFilePath -> DM (Maybe Int)
shibuCodeM mfp = do
  d <- (pwd =<< shibud =<< mfp)
  return $ readMaybe $ take 2 d

officePartsM :: MonadFilePath -> DM (String, String)
officePartsM mfp = do
  d <- (pwd =<< officed =<< mfp)
  case splitOn "_" d of
    [c, n] -> return (c, n)
    _      -> return ("", "")

monadicPath :: FilePath -> MonadFilePath
monadicPath fp = return $ (V.fromList $ splitOn "/" fp, 0)

data SendType  = Get  | Lost | Other deriving (Eq, Ord, Show)

data Send = S { code      :: String
              , name      :: String
              , sendType  :: SendType
              , hihoName  :: String
              , shibu     :: Maybe Int } deriving (Show, Eq, Ord)

data TreeDirectory = TD { shibuCode :: Maybe Int
                        , tdPath    :: FilePath
                        , oCode     :: String
                        , oName     :: String
                        , person    :: String
                        , isLost    :: Bool } deriving (Show, Eq)

instance Ord TreeDirectory where
  compare x y
    | x == y            = EQ
    | oCode x < oCode y = LT
    | otherwise         = GT

type XSend = Record
  '[ "code"      >: String
   , "name"      >: String
   , "sendType"  >: SendType
   , "hihoName"  >: String
   , "shibu"     >: Maybe Int ]

type XTD = Record
  '[ "shibuCode" >: Maybe Int
   , "filePath"  >: MonadFilePath
   , "oCode"     >: String
   , "oName"     >: String
   , "person"    >: String
   , "isLost"    >: Bool ]

makeSend :: [Text] -> Send
makeSend t = case t of
  [_c, _n, _st, _hn, _si, _sh] ->
    S { code      = unpack _c
      , name      = unpack $ officeTypeReplace $ killBlanks _n
      , sendType  = makeSendType _st
      , hihoName  = unpack $ killBlanks _hn
      , shibu     = toCode $ unpack _sh }
  _ -> error "must not happen"

makeXSend :: [Text] -> Either Text XSend
makeXSend t = case t of
  [_c, _n, _st, _hn, _si, _sh] ->
    Right (
      #code        @= unpack _c
      <: #name     @= (unpack $ officeTypeReplace $ killBlanks _n)
      <: #sendType @= makeSendType _st
      <: #hihoName @= (unpack $ killBlanks _hn)
      <: #shibu    @= (toCode $ unpack _sh)
      <: nil )
  _ -> Left $ "parse error: " <> intercalate "," t

makeSendType :: Text -> SendType
makeSendType t | "取得" `isInfixOf` t = Get
               | "喪失" `isInfixOf` t = Lost
               | otherwise           = Other

readData :: (MonadCatch m, MonadIO m) => m [Send]
readData = do
  filename <- sendCSVFileName
  spec     <- directorySpecF
  contents <- parseCSV2 spec filename
  return $ map makeSend contents

readSendFile :: IO [XSend]
readSendFile = do
  filename <- sendCSVFileName
  spec     <- directorySpecF
  contents <- map makeXSend <$> parseCSV2 spec filename
  forM_ (lefts contents) print
  return $ rights contents

makeTreeDirectory :: FilePath -> Maybe TreeDirectory
makeTreeDirectory fp =
  case splitOn "/" fp of
    [_, _, _, s, o, p, _] -> coreF s fp o p True
    [_, _, _, s, o, p]    -> coreF s fp o p False
    _                     -> Nothing
  where oParse o = case splitOn "_" o of
                     [c, n] -> Just (c, n)
                     _      -> Nothing
        sParse s = readMaybe $ take 2 s
        coreF s f o p b =
          case oParse o of
            Just (oc, on) -> Just $ TD (sParse s) f oc on p b
            Nothing       -> Nothing

makeXTD :: FilePath -> XTD
makeXTD fp =
  let mfp = monadicPath fp
  in let officeParts = runDM $ officePartsM mfp
  in #shibuCode   @= runDM (shibuCodeM mfp)
     <: #filePath @= mfp
     <: #oCode    @= fst officeParts
     <: #oName    @= snd officeParts
     <: #person   @= runDM (mfp >>= persond >>= pwd)
     <: #isLost   @= runDM (mfp >>= lostp)
     <: nil

makeXTDMap :: [XTD] -> XTDMap
makeXTDMap xtd =
  let insert mp el =
        M.insertWith (++) (el ^. #shibuCode) [el] mp
  in foldl insert M.empty xtd

getXTD :: (XTD -> Bool) -> IO [XTD]
getXTD f = do
  topdir <- fileTreeDirectory
  let conduit = sourceDescendDirectory topdir
                .| CL.map makeXTD
                .| CL.filter f
                .| CL.consume
  liftIO $ runConduit conduit

getXTDMap :: IO XTDMap
getXTDMap = do
  contents <- getXTD (\xtd -> runDM (xtd ^. #filePath >>= enoughLength))
  return $ makeXTDMap contents

makeTreeDirectoryMap :: [Maybe TreeDirectory] -> TreeDirectoryMap
makeTreeDirectoryMap td =
  let insert mp Nothing = mp
      insert mp (Just el) =
        M.insertWith (++) (shibuCode el) [(oCode el, person el, isLost el)] mp
  in foldl insert M.empty td

getTreeDirectory :: (MonadCatch m, MonadIO m)
  => (FilePath -> Bool)
  -> m [Maybe TreeDirectory]
getTreeDirectory f = do
  topdir <- fileTreeDirectory
  let conduit = sourceDescendDirectory topdir
                .| CL.filter f
                .| CL.map makeTreeDirectory
                .| CL.consume
  liftIO $ runConduit conduit

fileTree :: (MonadCatch m, MonadIO m) => m TreeDirectoryMap
fileTree = do
  contents <- getTreeDirectory (splitOn "/" >>> length >>> (>= 6))
  return $ makeTreeDirectoryMap contents

sourceDescendDirectory :: FilePath -> Source IO FilePath
sourceDescendDirectory fp = do
  directoryP <- lift $ doesDirectoryExist fp
  when directoryP $ do
    yield fp
    dirs <- lift $ listDirectory (fp ++ "/")
    mapM_ sourceDescendDirectory dirs

sourceDescendFile :: FilePath -> Source IO FilePath
sourceDescendFile fp = do
  directoryP <- liftIO $ doesDirectoryExist fp
  fileP      <- liftIO $ doesFileExist fp
  case (directoryP, fileP) of
    (True, _) -> do
      contents <- liftIO $ listDirectory (fp <> "/")
      forM_ contents sourceDescendFile
    (_, True) -> yield fp

hasTree :: TreeDirectoryMap -> Send -> Bool
hasTree tdMap send =
  case shibu send `M.lookup` tdMap of
    Just shibuAlist ->
      let bool = sendType send /= Get
      in (code send, hihoName send, bool) `elem` shibuAlist
    Nothing         -> False

sendAlreadyRegistered :: (XSend, Bool) -> [XTD] -> Bool
sendAlreadyRegistered _ [] = False
sendAlreadyRegistered (send, bool) (x:xs)
  | (send ^. #shibu) == (x ^. #shibuCode) &&
    (send ^. #hihoName) == (x ^. #person) = True
  | otherwise = (send, bool) `sendAlreadyRegistered` xs

hasTree2 :: XTDMap -> XSend -> Bool
hasTree2 xtdm send =
  case (send ^. #shibu) `M.lookup` xtdm of
    Just xtds ->
      let bool = send ^. #sendType /= Get
      in (send, bool) `sendAlreadyRegistered` xtds
    Nothing -> False

debugPrint :: Send -> String
debugPrint s =
  let sb = fromMaybe "0" $ show <$> shibu s
  in sb <> "_" <> code s <> "_" <> name s <> "_" <> hihoName s

shibuDirectoryName :: Send -> String
shibuDirectoryName s =
  case toShibu =<< shibu s of
    Just sb -> show (fromJust $ shibu s) <> "公文書(" <> sb <> ")"
    Nothing -> ""

officeDirectoryName :: Send -> String
officeDirectoryName s = code s <> "_" <> name s

personDirectoryName :: Send -> String
personDirectoryName = hihoName

directoryList :: Send -> [String]
directoryList s = basic ++ sendtype
  where
    sendtype | sendType s == Lost = ["喪失"]
             | otherwise = []
    basic = map ($ s) [ shibuDirectoryName
                      , officeDirectoryName
                      , personDirectoryName]

createDirectoryRecursive :: FilePath -> [String] -> IO ()
createDirectoryRecursive _ [] = return ()
createDirectoryRecursive fp (x:xs) = do
  let newPath = fp <> x <> "/"
  bool <- doesDirectoryExist newPath
  unless bool $ do
    putStrLn newPath
    print bool
    createDirectoryIfMissing True newPath
  createDirectoryRecursive newPath xs

createHihoDirectory :: IO ()
createHihoDirectory =
  ((>> return ()) . runExceptT) $ do
    fp <- fileTreeDirectory
    ft <- fileTree
    rd <- readData

    forM_ rd $ \n ->
      unless (ft `hasTree` n) $
        liftIO $ createDirectoryRecursive (fp <> "/") $ directoryList n

officeNameParser :: Parser (String, String)
officeNameParser =
  (,) <$> count 6 digit <* char '_' <*> many anyChar

officeNameP :: FilePath -> Bool
officeNameP = isRight . parse officeNameParser ""

officeNumber :: FilePath -> Maybe String
officeNumber fp = case parse officeNameParser "" bn of
                    Right (n, _) -> Just n
                    _            -> Nothing
  where
    bn = basename fp

depth :: FilePath -> Int
depth = length . splitDirectories

basename :: FilePath -> String
basename = takeBaseName

officeP, personalP, lostP :: FilePath -> Bool
officeP   d = (depth d == 5) && officeNameP (basename d)
personalP d = (depth d == 6) && officeNameP (basename $ takeDirectory d)
lostP     d = (depth d == 7) && ((=="喪失") $ basename d)

targetDirectories :: Conduit FilePath IO FilePath
targetDirectories =
  awaitForever $ \dir ->
    when (personalP dir || officeP dir || lostP dir) $ do
      len <- liftIO (length <$> getDirectoryContents dir)
      when (len == 2) $ yield dir

removeBlankDirectorySink :: Sink FilePath IO ()
removeBlankDirectorySink = do
  targets <- CL.consume
  mapM_ (liftIO . putStrLn) targets
  liftIO $ putStrLn "Delete All Files (y/n)?"
  liftIO $ hFlush stdout
  let quiz = do
        c <- liftIO getChar
        case c of
          'y' -> forM_ targets $ \directory -> do
            liftIO $ removeDirectory directory
            liftIO $ putStrLn $ directory ++ " deleted."
          'n' -> return ()
          _   -> do
            putStrLn "answer y or n"
            quiz
  lift quiz

removeBlankDirectory :: IO ()
removeBlankDirectory = do
  Right t <- runExceptT fileTreeDirectory
  sourceDescendDirectory t
    $= targetDirectories
    --- $$ removeBlankDirectorySink
    $$ CL.mapM_ putStrLn

duplicateOfficeCheck :: IO ()
duplicateOfficeCheck = do
  Right t <- runExceptT fileTreeDirectory
  let dupListConduit = do
        xl <- CL.consume
        let l = M.toList $ makeListMap officeNumber (:[]) xl
        mapM_ yield l
  let dupFilter =
        awaitForever $ \(_, val) ->
          when (length val /= 1) $ yield val
  let dupSink = do
        xl <- CL.consume
        mapM_ (liftIO . putStrLn) $ concat xl
  runConduit
    $ sourceDescendDirectory t
    .| CL.filter officeP
    .| dupListConduit
    .| dupFilter
    .| dupSink
