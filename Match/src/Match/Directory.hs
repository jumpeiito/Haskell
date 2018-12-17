{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Match.Directory
  (createHihoDirectory, removeBlankDirectory, directoryTest) where

import Foreign.C.Types
import Foreign.C.String

import           Control.Arrow              ((>>>))
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Either                (isRight)
import           Data.List.Split            (splitOn)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, isInfixOf, unpack)
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
                                            , getDirectoryContents
                                            , removeDirectory)
import           System.FilePath
import           System.IO                  (hFlush, stdout)
import           Text.Parsec
import           Text.Parsec.String
import           Text.Read                  (readMaybe)
import           Util                       (listDirectory, makeListMap)

type TreeDirectoryMap = M.Map (Maybe Int) [(String, String, Bool)]

data Situation = Done | Yet deriving (Eq, Ord, Show)
data SendType  = Get  | Lost | Other deriving (Eq, Ord, Show)

data Send = S { code      :: String
              , name      :: String
              , sendType  :: SendType
              , hihoName  :: String
              , situation :: Situation
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

makeSend :: [Text] -> Send
makeSend t = case t of
  [_c, _n, _st, _hn, _si, _sh] ->
    S { code      = unpack _c
      , name      = unpack $ officeTypeReplace $ killBlanks _n
      , sendType  = makeSendType _st
      , hihoName  = unpack $ killBlanks _hn
      , situation = makeSituation _si
      , shibu     = toCode $ unpack _sh }
  _ -> error "must not happen"

makeSendType :: Text -> SendType
makeSendType t | "取得" `isInfixOf` t = Get
               | "喪失" `isInfixOf` t = Lost
               | otherwise           = Other

makeSituation :: Text -> Situation
makeSituation t | "手続終了" `isInfixOf` t = Done
                | otherwise = Yet

readData :: (MonadCatch m, MonadIO m) => m [Send]
readData = do
  filename <- sendCSVFileName
  spec     <- directorySpecF
  contents <- parseCSV2 spec filename
  return $ map makeSend contents

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
                =$ CL.filter f
                =$ CL.map makeTreeDirectory
                $$ CL.consume
  liftIO conduit

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

hasTree :: TreeDirectoryMap -> Send -> Bool
hasTree tdMap send =
  case shibu send `M.lookup` tdMap of
    Just shibuAlist ->
      let bool = sendType send /= Get
      in (code send, hihoName send, bool) `elem` shibuAlist
    Nothing         -> False

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
        awaitForever $ \(key, val) ->
          when (length val /= 1) $ yield val
  let dupSink = do
        xl <- CL.consume
        mapM_ (liftIO . putStrLn) $ concat xl
  sourceDescendDirectory t
    $= CL.filter officeP
    $= dupListConduit
    $= dupFilter
    $$ dupSink
