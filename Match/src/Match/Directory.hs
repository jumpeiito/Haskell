{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}
module Match.Directory where
  -- (createHihoDirectory, removeBlankDirectory) where

import           Control.Lens
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.State        (get)
import           Control.Monad.Trans        (lift, liftIO)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Either                (isRight, lefts, rights)
import           Data.Extensible
import           Data.List.Split            (splitOn)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           Data.Text                  ( Text
                                            , isInfixOf
                                            , intercalate
                                            , unpack)
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
import           System.IO                  (hFlush, stdout)
import           Text.Parsec
import           Text.Parsec.String
import           Text.Read                  (readMaybe)
import           Util                       (listDirectory)
import           Util.MonadPath

type XTDMap = M.Map (Maybe Int) [XTD]

shibud, officed, persond :: MonadicPath ()
shibud  = initialM >> repM 4 downM
officed = initialM >> repM 5 downM
persond = initialM >> repM 6 downM

lostp, enoughp :: MonadicPath Bool
lostp   = isLengthp 7
enoughp = (>= 6) <$> (V.length . fst) <$> get

shibuCodeM :: MonadicPath (Maybe Int)
shibuCodeM = do
  d <- shibud >> basenameM
  return $ readMaybe $ take 2 d

officePartsM :: MonadicPath (String, String)
officePartsM = do
  d <- officed >> basenameM
  case splitOn "_" d of
    [c, n] -> return (c, n)
    _      -> return ("", "")

officeNameParser :: Parser (String, String)
officeNameParser =
  (,) <$> count 6 digit <* char '_' <*> many anyChar

officeNameP :: FilePath -> Bool
officeNameP = isRight . parse officeNameParser ""

officeMP, personalMP, lostMP, targetMP :: MonadicPath Bool
officeMP = do
  len <- isLengthp 5
  bn  <- bottomM >> basenameM
  return $ len && officeNameP bn

personalMP = do
  len <- isLengthp 6
  bnu <- bottomM >> (../) >> basenameM
  return $ len && officeNameP bnu

lostMP = do
  len <- isLengthp 7
  bnu <- bottomM >> basenameM
  return $ len && (bnu == "喪失")

targetMP = do
  bools <- sequence [officeMP, personalMP, lostMP]
  return $ or bools

data SendType  = Get  | Lost | Other deriving (Eq, Ord, Show)

type XSend = Record
  '[ "code"      >: String
   , "name"      >: String
   , "sendType"  >: SendType
   , "hihoName"  >: String
   , "shibu"     >: Maybe Int ]

xsendShibuName :: XSend -> String
xsendShibuName s =
  case toShibu =<< s ^. #shibu of
    Just sb -> show (fromJust $ s ^. #shibu) <> "公文書(" <> sb <> ")"
    Nothing -> ""

xsendOfficeName :: XSend -> String
xsendOfficeName s = s ^. #code <> "_" <> s ^. #name

xsendPersonName :: XSend -> String
xsendPersonName = (^. #hihoName)

xsendDirectoryList :: XSend -> [String]
xsendDirectoryList s = basic ++ sendtype
  where
    sendtype | s ^. #sendType == Lost = ["喪失"]
             | otherwise = []
    basic = map ($ s) [ xsendShibuName
                      , xsendOfficeName
                      , xsendPersonName]

type XTD = Record
  '[ "shibuCode" >: Maybe Int
   , "filePath"  >: FilePath
   , "oCode"     >: String
   , "oName"     >: String
   , "person"    >: String
   , "isLost"    >: Bool ]

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

readSendFile :: IO [XSend]
readSendFile = do
  filename <- sendCSVFileName
  spec     <- directorySpecF
  contents <- map makeXSend <$> parseCSV2 spec filename
  forM_ (lefts contents) print
  return $ rights contents

makeXTD :: FilePath -> XTD
makeXTD fp =
  runFileM fp $ do
    officeParts <- officePartsM
    shibuCode   <- shibuCodeM
    person      <- persond >> basenameM
    isLost      <- lostp
    return $
      #shibuCode   @= shibuCode
      <: #filePath @= fp
      <: #oCode    @= fst officeParts
      <: #oName    @= snd officeParts
      <: #person   @= person
      <: #isLost   @= isLost
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
  contents <- getXTD ((`runFileM` enoughp) . (^. #filePath))
  return $ makeXTDMap contents

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

sendAlreadyRegistered :: (XSend, Bool) -> [XTD] -> Bool
sendAlreadyRegistered _ [] = False
sendAlreadyRegistered (send, bool) (x:xs)
  | (send ^. #shibu) == (x ^. #shibuCode) &&
    (send ^. #hihoName) == (x ^. #person) = True
  | otherwise = (send, bool) `sendAlreadyRegistered` xs

hasTree2 :: XSend -> XTDMap -> Bool
hasTree2 send xtdm =
  case (send ^. #shibu) `M.lookup` xtdm of
    Just xtds ->
      let bool = send ^. #sendType /= Get
      in (send, bool) `sendAlreadyRegistered` xtds
    Nothing -> False

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
createHihoDirectory = do
  fp   <- fileTreeDirectory
  xmap <- getXTDMap
  xsnd <- readSendFile
  forM_ xsnd $ \n -> do
    print n
    unless (n `hasTree2` xmap) $
      createDirectoryRecursive (fp <> "/") $ xsendDirectoryList n

removeTargetDirectories :: Conduit FilePath IO FilePath
removeTargetDirectories = do
  awaitForever $ \dir -> do
    when (dir `runFileM` targetMP) $ do
      len <- liftIO (length <$> getDirectoryContents dir)
      when (len == 2) $
        yield dir

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
  topPath  <- fileTreeDirectory
  let producer = sourceDescendDirectory topPath
                 $= removeTargetDirectories
  len <- producer $$ (length <$> CL.consume)
  if len == 0
    then return ()
    else do producer $$ removeBlankDirectorySink
            removeBlankDirectory
