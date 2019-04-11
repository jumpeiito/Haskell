{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Match.Directory
  ( createHihoDirectory
  , removeBlankDirectory
  , openPDFFileFromString
  , openPDFFileSexp
  , openPDFFileToday) where

import           Control.Applicative   ((<|>))
import           Control.Arrow         ((>>>))
import           Control.Lens
import           Control.Monad         (forM_, unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.State   (get)
import           Control.Monad.Trans   (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Either           (isRight, lefts, rights)
import           Data.Extensible
import qualified Data.List             as DL
import           Data.List.Split       (splitOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             ( Text
                                       , pack
                                       , isInfixOf
                                       , intercalate
                                       , unpack)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Vector           ((!?))
import qualified Data.Vector           as V
import           Database.Persist      hiding (get)
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import           Match.Base              ( killBlanks
                                         , officeTypeReplace
                                         , toCode
                                         , toShibu)
import           Match.Config            ( directorySpecF
                                         , fileTreeDirectory
                                         , sendCSVFileName)
import           Match.CSV               (parseCSV2)
import           System.Directory        ( createDirectoryIfMissing
                                         , doesDirectoryExist
                                         , doesFileExist
                                         , getDirectoryContents
                                         , getModificationTime
                                         , removeFile
                                         , removeDirectory)
import           System.IO               (hFlush, stdout
                                         , writeFile)
import           System.FilePath.Posix   (takeExtension)
import           System.Process          (runInteractiveCommand)
import           Text.Heredoc
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P
import           Text.Read               (readMaybe)
import           Util
import           Util.Strdt
import           Util.MonadPath

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Unit
    mtime Text
    kind Text
    number Text
    shibuCode Text
    officeCode Text
    officeName Text
    person Text
    deriving Show
|]

dbPath :: FilePath
dbPath = "d:/home/.directory.sqlite3"

makeNewDB :: IO ()
makeNewDB = do
  p <- doesFileExist dbPath
  when p $ removeFile dbPath
  runSqlite (pack dbPath) $ runMigration migrateAll

insertDB :: Unit -> IO ()
insertDB u = do
  runSqlite (pack dbPath) . s $ insert u >> return ()

s :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
s = id

type DirectoryBaseInfo = (Maybe Int, (String, String), String, Bool)

shibud, officed, persond :: MonadicPath ()
shibud  = initialM >> 3 `repM` downM
officed = initialM >> 4 `repM` downM
persond = initialM >> 5 `repM` downM

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

officeNameParser :: P.Parser (String, String)
officeNameParser =
  (,) <$> P.count 6 P.digit <* P.char '_' <*> P.many P.anyChar

officeNameP :: FilePath -> Bool
officeNameP = isRight . P.parse officeNameParser ""

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
               | otherwise            = Other

readSendFile :: IO [XSend]
readSendFile = do
  filename <- sendCSVFileName
  spec     <- directorySpecF
  contents <- map makeXSend <$> parseCSV2 spec filename
  forM_ (lefts contents) print
  return $ rights contents

sourceDescendDirectory :: FilePath -> Source IO FilePath
sourceDescendDirectory fp = do
  directoryP <- liftIO $ doesDirectoryExist fp
  when directoryP $ do
    yield fp
    dirs <- liftIO $ listDirectory (fp ++ "/")
    mapM_ sourceDescendDirectory dirs

createDirectoryRecursive :: FilePath -> [String] -> IO ()
createDirectoryRecursive _ [] = return ()
createDirectoryRecursive fp (x:xs) = do
  let newPath = fp <> x <> "/"
  bool <- doesDirectoryExist newPath
  unless bool $ do
    putStrLn newPath
    createDirectoryIfMissing True newPath
  createDirectoryRecursive newPath xs

createHihoDirectory :: IO ()
createHihoDirectory = do
  fp   <- fileTreeDirectory
  xsnd <- readSendFile
  forM_ xsnd $ \n -> do
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

directoryBaseInfoList :: DirectoryBaseInfo -> [String]
directoryBaseInfoList dbi =
  let (shibu, (oCode, oName), person, bool) = dbi
  in let shibu' = show $ fromMaybe 99 shibu
  in let pdftype = if bool then "喪失" else "取得"
  in [oCode, oName, shibu', person, pdftype]

type PDFVector = V.Vector (String, [FilePath])

makePDFVector :: [FilePath] -> PDFVector
makePDFVector =
  M.toList . baseInfoMap
  >>> map (\(k, v) -> (directoryBaseInfoString k, fromDiffList v))
  >>> V.fromList

pdfVectorFirstOutput :: MonadIO m => PDFVector -> m ()
pdfVectorFirstOutput pv = do
  let indexPV = V.indexed pv
  let tostr (idx, (k, _)) = "(" ++ show idx ++ ")" ++ k
  forM_ indexPV $ \v -> liftIO $ do
    putStrLn (tostr v)
    hFlush stdout

pdfOpenFromVector :: MonadIO m => PDFVector -> Int -> m ()
pdfOpenFromVector pv i = do
  case pv !? i of
    Nothing      -> return ()
    Just (_, xl) -> forM_ xl (liftIO . openPDFFileCommand)

pdfSink2 :: Sink FilePath IO ()
pdfSink2 = do
  pdflist <- CL.consume
  let infoVector = makePDFVector pdflist
  pdfVectorFirstOutput infoVector
  range <- liftIO $ getLine
  case fromRange range of
    Nothing -> return ()
    Just xl -> forM_ xl (pdfOpenFromVector infoVector)

pdfSinkSexp :: Sink FilePath IO ()
pdfSinkSexp = do
  pdflist <- CL.consume
  let finalize =
        baseInfoMap
        >>> M.toList
        >>> map (\(k, v) -> directoryBaseInfoList k
                            ++ fromDiffList v)
        >>> map (map (\n -> "\"" <> n <> "\""))
        >>> map (DL.intercalate " ")
        >>> map (\n -> "(" ++ n ++ ")")
        >>> DL.zip [0..]
        >>> map (\(n, s) -> "(" ++ show n ++ " . " ++ s ++ ")")
  let final = DL.intercalate "\n" $ finalize pdflist
  liftIO $ writeFile "d:/home/.sexp" $ "(" ++ final ++ ")"

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
    .| pdfSink2

openPDFFileSexp :: String -> String -> IO ()
openPDFFileSexp dayString diffDay = do
  case (strdt dayString :: Maybe Day) of
    Nothing   -> return ()
    Just pday -> do
      topPath <- fileTreeDirectory
      runConduit $
        pSearchSourceRecently True diffDay topPath
        .| CL.filter (takeExtension >>> (== ".pdf"))
        .| dayFilter pday
        .| pdfSinkSexp

openPDFFileFromString :: String -> IO ()
openPDFFileFromString dayString = do
  case strdt dayString of
    Just d  -> openPDFFile d
    Nothing -> return ()

openPDFFileToday :: IO ()
openPDFFileToday = do
  today' <- todayDay
  openPDFFile today'

hasHyphenParser :: P.Parser [Int]
hasHyphenParser = do
  beg <- read <$> P.many1 P.digit <* (P.char '-')
  end <- read <$> P.many1 P.digit
  return $ [beg..end]

onlyNumParser :: P.Parser [Int]
onlyNumParser = do
  (:[]) <$> read <$> P.many1 P.digit

trimSpace :: P.Parser a -> P.Parser a
trimSpace p = do
  let space = P.many (P.char ' ')
  let maybeSpace = P.optional space
  P.between maybeSpace maybeSpace p

rangeParse :: P.Parser [Int]
rangeParse = do
  let parser =
        P.try (trimSpace hasHyphenParser)
        <|> (trimSpace onlyNumParser)
  let postFunc = DL.sort . DL.nub . mconcat
  let sep    = P.char ','
  intList <- P.sepBy1 parser sep
  return $ postFunc intList

fromRange :: String -> Maybe [Int]
fromRange target =
  case P.parse rangeParse "" target of
    Right x -> Just x
    Left _  -> Nothing

pSearchSourceRecently ::
  Bool -> String -> FilePath -> Source IO FilePath
pSearchSourceRecently flg diffDay fp = do
  p  <- liftIO $ doesDirectoryExist fp
  if p
    then
    do cond <- liftIO $ pSSRCondition flg diffDay fp
       if cond
         then do contents <- liftIO $ listDirectory (fp <> "/")
                 forM_ contents (pSearchSourceRecently False diffDay)
         else return ()
    else yield fp

pSSRCondition ::
  Bool -> String -> FilePath -> IO Bool
pSSRCondition flg diffDay fp = do
  today' <- todayDay
  mtime  <- utctDay <$> getModificationTime fp
  case (readMaybe diffDay :: Maybe Integer) of
    Nothing -> return (flg || True)
    Just i  -> return (flg || diffDays today' mtime < i)
