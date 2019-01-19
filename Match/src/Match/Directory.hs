{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}
module Match.Directory
  (createHihoDirectory
  , removeBlankDirectory
  , openPDFFileFromString
  , openPDFFileToday) where

import           Control.Arrow              ((>>>))
import           Control.Lens
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.State        (get)
import           Control.Monad.Trans        (liftIO)
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
import           Data.Time.Calendar
import           Data.Time.Clock
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
                                            , getModificationTime
                                            , removeDirectory)
import           System.IO                  (hFlush, stdout)
import           System.FilePath.Posix      (takeExtension)
import           System.Process             (runInteractiveCommand)
import           Text.Heredoc
import           Text.Parsec
import           Text.Parsec.String
import           Text.Read                  (readMaybe)
import           Util
import           Util.Strdt
import           Util.MonadPath

type XTDMap = M.Map (Maybe Int) [XTD]
type DirectoryBaseInfo = (Maybe Int, (String, String), String, Bool)

shibud, officed, persond :: MonadicPath ()
shibud  = initialM >> 3 `repM` downM
officed = initialM >> 4 `repM` downM
persond = initialM >> 5 `repM` downM

lostp, enoughp :: MonadicPath Bool
lostp   = isLengthp 7
enoughp = (>= 6) <$> (V.length . fst) <$> get

shibuCodeM :: MonadicPath (Maybe Int)
shibuCodeM =
  readMaybe . (take 2) <$> (shibud >> basenameM)

officePartsM :: MonadicPath (String, String)
officePartsM = do
  d <- officed >> basenameM
  case splitOn "_" d of
    [c, n] -> return (c, n)
    _      -> return ("", "")

lostPDFP :: MonadicPath Bool
lostPDFP = do
  x <- get
  return $ "喪失" `V.elem` fst x

baseInfoM :: MonadicPath DirectoryBaseInfo
baseInfoM = do
  s <- shibuCodeM
  o <- officePartsM
  p <- persond >> basenameM
  b <- lostPDFP
  return (s, o, p, b)

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

targetMP = or <$> sequence [officeMP, personalMP, lostMP]

data SendType  = Get | Lost | Other deriving (Eq, Ord, Show)

toXSend :: DirectoryBaseInfo -> XSend
toXSend (s, (oC, oN), p, b) =
  #code @= oC
  <: #name @= oN
  <: #sendType @= (if b then Lost else Get)
  <: #hihoName @= p
  <: #shibu @= s
  <: nil

xsendToPath :: FilePath -> XSend -> FilePath
xsendToPath root xs =
  root ++ (DL.intercalate "/" $ xsendDirectoryList xs)

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
  getXTD ((`runFileM` enoughp) . (^. #filePath)) ===>
    Key (^. #shibuCode) `MakeListMap` Value id

sourceDescendDirectory :: FilePath -> Source IO FilePath
sourceDescendDirectory fp = do
  directoryP <- liftIO $ doesDirectoryExist fp
  when directoryP $ do
    yield fp
    dirs <- liftIO $ listDirectory (fp ++ "/")
    mapM_ sourceDescendDirectory dirs

sourceDescendFile :: FilePath -> Source IO FilePath
sourceDescendFile fp = do
  directoryP <- liftIO $ doesDirectoryExist fp
  fileP'     <- liftIO $ doesFileExist fp
  case (directoryP, fileP') of
    (True, _) -> do
      contents <- liftIO $ listDirectory (fp <> "/")
      forM_ contents sourceDescendFile
    (_, True) -> yield fp
    (False, False) -> error "must not happen."

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
          'y' -> forM_ targets $ \directory -> liftIO $ do
            removeDirectory directory
            putStrLn $ directory ++ " deleted."
          'n' -> return ()
          _   -> do
            putStrLn "answer y or n"
            quiz
  liftIO quiz

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

acrord :: FilePath
acrord = "\"c:/Program Files/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\""

openPDFFileCommand :: FilePath -> IO ()
openPDFFileCommand fp =
  runInteractiveCommand (acrord <> " " <> fp <> " &")
    >> return ()

pdfSink :: Sink String IO ()
pdfSink = do
  awaitForever $ \pdf -> liftIO $ do
    putStrLn pdf
    openPDFFileCommand pdf

baseInfoMap ::
  [FilePath] -> M.Map DirectoryBaseInfo (DiffList FilePath)
baseInfoMap dirs =
  dirs ==> Key (`runFileM` baseInfoM) `MakeDiffListMap` Value id

directoryBaseInfoString :: DirectoryBaseInfo -> String
directoryBaseInfoString dbi =
  let (shibu, (oCode, oName), person, bool) = dbi
  in let shibu' = show $ fromMaybe 99 shibu
  in let pdftype = if bool then "喪失" else "取得"
  in [heredoc|${oCode}-${oName}(${shibu'}) ${person}--${pdftype}|]

pdfCollectiveSink :: Sink String IO ()
pdfCollectiveSink = do
  pdflist <- CL.consume
  let infoMap = baseInfoMap pdflist
  forM_ (M.keys infoMap) $ \info -> liftIO $ do
    putStrLn $ directoryBaseInfoString info
    hFlush stdout
    c <- getLine
    case c of
      "n" -> return ()
      _   ->
        runConduit $
          CL.sourceList
            ((fromDiffList . fromJust) $ info `M.lookup` infoMap)
          .| pdfSink

dayFilter :: Day -> Conduit FilePath IO FilePath
dayFilter pday = do
  awaitForever $ \pdf -> do
    utc <- liftIO $ getModificationTime pdf
    when (utctDay utc == pday) $ do
      yield pdf

openPDFFile :: Day -> IO ()
openPDFFile pday = do
  topPath <- fileTreeDirectory
  runConduit $
    pSearchSource topPath
    .| CL.filter (takeExtension >>> (== ".pdf"))
    .| dayFilter pday
    .| pdfCollectiveSink

openPDFFileFromString :: String -> IO ()
openPDFFileFromString dayString = do
  case strdt dayString of
    Just d -> openPDFFile d
    Nothing -> return ()

openPDFFileToday :: IO ()
openPDFFileToday = do
  today' <- todayDay
  openPDFFile today'
