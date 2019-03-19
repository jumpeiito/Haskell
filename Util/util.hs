{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Control.Arrow
import           Control.Parallel.Strategies (parMap, rseq)
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.List
import           Data.String
import           Data.IORef
import           Data.Maybe
import qualified Data.Set                    as S
import           Control.Exception           hiding (try)
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.State.Strict
import           Control.Concurrent.Async
import           System.Directory            hiding (listDirectory)
import           System.Process
import           Text.StringLike             (StringLike)
import qualified Data.Map.Strict             as Map
import qualified System.IO                   as I
import qualified Data.Text                   as Tx
import qualified Data.Text.IO                as Txio
import qualified Data.Text.Internal          as Txi
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import           Text.Parsec                 hiding (State)
import           Text.Parsec.String
----------------------------------------------------------------------------------------------------
uniq :: Ord a => [a] -> [a]
uniq = fst . foldr uniq' ([], S.empty)
  where
    uniq' el (l, s) = if el `S.member` s
                      then (l, s)
                      else (el : l, el `S.insert` s)

ordNub :: Ord a => [a] -> [a]
ordNub xs = foldr (\x k s -> if S.member x s
  then k s
  else x : k (S.insert x s))
  (const []) xs S.empty

class StringLike a => Join a where
  joiner :: String -> [a] -> a

instance Join String where
  joiner _ [] = ""
  joiner _ [x] = x
  joiner glue (x:y:xs) = x <> glue <> y <> rest
    where rest | null xs   = ""
               | otherwise = glue <> joiner glue xs
----------------------------------------------------------------------------------------------------
data FileDirect =
  FD { dirP  :: FilePath -> Bool,
       fileP :: FilePath -> Bool }

(<~>), (<^>), (<!~>), (<!^>) :: FilePath -> FilePath -> Bool
(<~>) = isInfixOf
(<^>) = isSuffixOf
(<!~>) a b = not $ isInfixOf a b
(<!^>) a b = not $ isSuffixOf a b

allf :: FilePath -> IO [FilePath]
allf fp = allfd fp (FD (const True) (const True))

allfd :: FilePath -> FileDirect -> IO [FilePath]
allfd fp fd = snd <$> runWriterT (_allfd fp fd)

_allfd :: FilePath -> FileDirect -> WriterT [FilePath] IO ()
_allfd fp fd = _all_base fp fd f
  where f path bool = do
          case (bool, dirP fd fp, fileP fd path) of
            (True, _, _)        -> do { r <- liftIO $ allfd path fd; tell r }
            (False, True, True) -> tell [path]
            (False, _, _)       -> tell mempty

_all_base :: FilePath ->
             FileDirect ->
             (FilePath -> Bool -> WriterT [FilePath] IO b) ->
             WriterT [FilePath] IO ()
_all_base fp _ f = do
  let filtering = filter (`notElem` [".", ".."])
  let makePath  = map (\n -> fp ++ "/" ++ n)
  let cut       = makePath . filtering
  paths <- liftIO $ cut <$> getDirectoryContents fp
  forM_ paths $ \path -> do
    bool <- liftIO $ doesDirectoryExist path
    f path bool
----------------------------------------------------------------------------------------------------
_alld :: FilePath -> WriterT [FilePath] IO ()
_alld fp = _all_base fp fdn f
  where fdn = FD (const True) (const True)
        f path bool = do
          when bool $ do
            descend <- liftIO $ alld path
            tell $ [path] ++ descend

alld :: FilePath -> IO [FilePath]
alld fp = snd <$> runWriterT (_alld fp)
----------------------------------------------------------------------------------------------------
makeMap :: Ord k => (t -> k) -> (t -> a) -> [t] -> Map.Map k [a]
makeMap _ _ [] = Map.empty
makeMap kF vF (x:xs) =
  Map.insertWith (++) (kF x) [vF x] $ makeMap kF vF xs

makeSingleMap :: Ord k => (t -> k) -> (t -> a) -> [t] -> Map.Map k a
makeSingleMap _ _ [] = Map.empty
makeSingleMap kF vF (x:xs) =
  Map.insert (kF x) (vF x) $ makeSingleMap kF vF xs

makeCountMap :: (Num a, Ord k) => (t -> k) -> [t] -> Map.Map k a
makeCountMap _ [] = Map.empty
makeCountMap kF (x:xs) =
   Map.insertWith (+) (kF x) 1 $ makeCountMap kF xs

makeSumMap :: (Num a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map.Map k a
makeSumMap _ _ [] = Map.empty
makeSumMap kF vF (x:xs) =
   Map.insertWith (+) (kF x) (vF x) $ makeSumMap kF vF xs

makeListMap :: Ord k => (t -> k) -> (t -> [a]) -> [t] -> Map.Map k [a]
makeListMap _ _ [] = Map.empty
makeListMap kF vF (x:xs) =
   Map.insertWith (++) (kF x) (vF x) $ makeListMap kF vF xs

maybeS :: (IsString a, Monoid a) => Maybe a -> a
maybeS = (mempty `fromMaybe`)

xShow :: Show a => IsString b => a -> b
xShow =  fromString . show

class ReadFile a where
  readUTF8     :: FilePath -> IO a
  readUTF8line :: FilePath -> IO [a]
  readSJIS     :: FilePath -> IO a
  readSJISline :: FilePath -> IO [a]

baseReadFile :: String -> (I.Handle -> IO a) -> FilePath -> IO a
baseReadFile coding f fp =
  bracket (I.openFile fp I.ReadMode)
          -- (I.hClose)
          (const $ return ())
          (\h -> do
              encoding <- I.mkTextEncoding coding
              I.hSetEncoding h encoding
              f h >>= evaluate
          )

baseReadUTF8 :: (I.Handle -> IO a) -> FilePath -> IO a
baseReadUTF8 = baseReadFile "cp65001"

baseReadSJIS :: (I.Handle -> IO a) -> FilePath -> IO a
baseReadSJIS = baseReadFile "cp932"

instance ReadFile String where
  readUTF8        = baseReadUTF8 I.hGetContents
  readUTF8line fp = lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS I.hGetContents
  readSJISline fp = lines <$> readSJIS fp

instance ReadFile B.ByteString where
  readUTF8        = baseReadUTF8 B.hGetContents
  readUTF8line fp = B.lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS B.hGetContents
  readSJISline fp = B.lines <$> readSJIS fp

instance ReadFile BL.ByteString where
  readUTF8        = baseReadUTF8 BL.hGetContents
  readUTF8line fp = BL.lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS BL.hGetContents
  readSJISline fp = BL.lines <$> readSJIS fp

instance ReadFile Txi.Text where
  readUTF8        = baseReadUTF8 Txio.hGetContents
  readUTF8line fp = Tx.lines <$> readUTF8 fp
  readSJIS        = baseReadSJIS Txio.hGetContents
  readSJISline fp = Tx.lines <$> readSJIS fp

readUTF8File :: FilePath -> IO String
readUTF8File fp = do
  h <- I.openFile fp I.ReadMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hGetContents h

readUTF8ByteFile :: FilePath -> IO B.ByteString
readUTF8ByteFile fp = do
  h <- I.openFile fp I.ReadMode
  I.hSetEncoding h I.utf8
  B.hGetContents h

sjisLines :: FilePath -> IO [String]
sjisLines fp = do
  -- encoding <- I.mkTextEncoding "cp932"
  bracket (I.openFile fp I.ReadMode) (const $ return ()) $ \h -> do
    I.hSetEncoding h I.utf8
    lines <$> I.hGetContents h
    -- return $! lines content

withOutFile :: FilePath -> (I.Handle -> IO ()) -> IO ()
withOutFile oFile func = do
  h <- I.openFile oFile I.WriteMode
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  func h
  I.hClose h

withAppendFile :: FilePath -> (I.Handle -> IO ()) -> IO ()
withAppendFile oFile func = do
  bool <- doesFileExist oFile
  let mode | bool      = I.AppendMode
           | otherwise = I.WriteMode
  bracket (I.openFile oFile mode) (I.hClose) $ \h -> do
    encoding <- I.mkTextEncoding "cp65001"
    I.hSetEncoding h encoding
    func h

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File fp contents = do
  h <- (I.openFile fp I.WriteMode)
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hPutStrLn h contents
  I.hClose h

appendUTF8File :: FilePath -> String -> IO ()
appendUTF8File fp contents = do
  h <- (I.openFile fp I.AppendMode)
  encoding <- I.mkTextEncoding "cp65001"
  I.hSetEncoding h encoding
  I.hPutStrLn h contents
  I.hClose h

(</>) :: FilePath -> String -> FilePath
(</>) dirname filename =
  if "/" `isSuffixOf` dirname
  then dirname ++ filename
  else dirname ++ "/" ++ filename


_include :: [String] -> Parser String
_include xs = do
  try $ choice $ map (try . string) xs
  <|> (anyChar >> _include xs)

include :: [String] -> String -> Bool
include xs target = either (const False) (const True)
                           $ parse (_include xs) "" target

(++++) :: Parser String -> Parser String -> Parser String
(++++) p1 p2 = (++) <$> p1 <*> p2

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n x = (drop n x) ++ (take n x)

runRuby :: [String] -> IO (I.Handle, I.Handle, I.Handle, ProcessHandle)
runRuby opt = do
  runInteractiveProcess "ruby" opt Nothing Nothing

runRubyString :: [String] -> IO [String]
runRubyString opt = do
  (_, sout, _, _) <- runRuby opt
  lines <$> I.hGetContents sout

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l =
  let answer (_, (_, _, x)) = reverse x in
  answer . (`runState` (0, [], [])) $ do
    forM_ l $ \el -> do
      (counter, small, big) <- get
      if n == counter
        then put (1, [el], small : big)
        else put (counter + 1, small ++ [el], big)
    (_, small, big) <- get
    put (0, [], small : big)

ketaNum :: String -> String
ketaNum str = reverse $ intercalate "," $ Util.group 3 $ reverse str
----------------------------------------------------------------------------------------------------
data Location = LocHome | LocOffice | LocOther deriving (Show, Eq)

whereLoc :: IO Location
whereLoc = do
  hBool <- doesDirectoryExist "c:/Users/Jumpei"
  oBool <- doesDirectoryExist "c:/Users/SIBUC526.NEWNET/"
  case (hBool, oBool) of
    (True, _) -> return LocHome
    (_, True) -> return LocOffice
    (_, _)    -> return LocOther

locEncoding :: IO ()
locEncoding = do
  loc <- whereLoc
  case loc of
    LocHome   -> I.hSetEncoding I.stdout I.utf8
    LocOffice -> do
      sjis <- I.mkTextEncoding "CP932"
      I.hSetEncoding I.stdout sjis
    LocOther  -> I.hSetEncoding I.stdout I.utf8

-- data ExcelCol = Column [Int] deriving Show

-- stringToColumn :: String -> ExcelCol
-- stringToColumn = Column . map (\ch -> ord ch - 64) . reverse

-- columnToInt :: ExcelCol -> Int
-- columnToInt col = sum $ map (uncurry (*)) $ zip exp' col'
--   where Column col' = col
--         exp' = [ truncate $ 26 ** x | x <- [0..] ]

-- columnDivide :: Int -> (Int, Int)
-- columnDivide i = case (i `mod` 26, i `div` 26) of
--                    (0, 0) -> (0, 0)
--                    (0, 1) -> (26, 0)
--                    (0, x) -> (26, x - 1)
--                    (y, d) -> (y, d)

-- intToExp :: Int -> [Int]
-- intToExp i = case columnDivide i of
--                (modulo, 0) -> [modulo]
--                (m, d) -> intToExp d ++ [m]

-- intToString :: Int -> String
-- intToString i = map (\n -> chr $ n + 64) $ intToExp i

scan :: Parser a -> Parser [a]
scan f1 = do
  try ((:) <$> f1 <*> scan f1)
  <|> (eof >> return [])        -- 終了条件
  <|> (anyChar >> scan f1)

data FileSystem = File [FilePath] | Directory [FilePath] deriving Show

runFile :: FileSystem -> IO (Maybe FilePath)
runFile (File []) = return Nothing
runFile (Directory []) = return Nothing
runFile (File (x:xs)) = do
  bool <- doesFileExist x
  if bool
    then return $ Just x
    else runFile (File xs)
runFile (Directory (x:xs)) = do
  bool <- doesDirectoryExist x
  if bool
    then return $ Just x
    else runFile (Directory xs)

latexCom :: String -> [String] -> String
latexCom comName args =
  "\\" ++ comName ++ concatMap enclose args
  where enclose s = "{" ++ s ++ "}"

latexEnv :: String -> [String] -> String
latexEnv envName args =
  "\\begin{" ++ envName ++ "}" ++ concatMap enclose args ++ "\\end{" ++ envName ++ "}"
  where enclose s = "{" ++ s ++ "}"

-- data Hoken     = Ordinary | Nenkin | Kaigo deriving (Eq, Ord, Bounded, Enum)
-- data HokenKind = New | Old deriving (Eq, Ord)
-- type HokenKey  = (Hoken, HokenKind)
-- type HokenCell = (Hoken, HokenKind, Rational)
-- data HokenCalcurator = HC { initialList :: [HokenCell]
--                           , answer      :: [(Hoken, Rational)]
--                           , hokenFee    :: Int
--                           } deriving (Show, Eq)

-- takeFeeMax :: [HokenCalcurator] -> [HokenCalcurator]
-- takeFeeMax alist = filter ((==) maxFee . hokenFee) alist
--   where maxFee = maximum $ map hokenFee alist

-- takeMax :: [HokenCalcurator] -> [HokenCalcurator]
-- takeMax alist = filter maxAndLeast alist
--   where maxFeeList     = takeFeeMax alist
--         maxFee         = maximum $ map hokenFee alist
--         leastSize      = minimum $ map (length . initialList) maxFeeList
--         maxAndLeast hc = maxFee == hokenFee hc && leastSize == length (initialList hc)

-- takeNth :: Int -> [HokenCalcurator] -> [HokenCalcurator]
-- takeNth n = take n . reverse . sort

-- instance Show Hoken where
--   show Ordinary = "o"
--   show Nenkin   = "n"
--   show Kaigo    = "k"

-- instance Show HokenKind where
--   show New = "."
--   show Old = "!"

-- instance Ord HokenCalcurator where
--   hc1 `compare` hc2 = if fee' == fee''
--                       then Down length' `compare` Down length''
--                       else fee' `compare` fee''
--     where makePair = (&&&) hokenFee (length . initialList)
--           (fee', length') = makePair hc1
--           (fee'', length'') = makePair hc2

-- makeHC :: [HokenCell] -> HokenCalcurator
-- makeHC hc = HC hc (sumHoken hc) (totalHoken hc)

-- kojoCalc :: (HokenKey, Rational) -> (HokenKey, Rational)
-- kojoCalc (hk, r) = (hk, kojo hk r)

-- kojo :: HokenKey -> Rational -> Rational
-- kojo (_, New) i = kojoNew i
-- kojo (_, Old) i = kojoOld i

-- kojoNew :: Rational -> Rational
-- kojoNew i | i <= 20000 = i
--           | i <= 40000 = i / 2 + 10000
--           | i <= 80000 = i / 4 + 20000
--           | otherwise   = 40000

-- kojoOld :: Rational -> Rational
-- kojoOld i | i <= 25000 = 0
--           | i <= 50000 = i / 2 + 12500
--           | i <= 100000 = i / 4 + 25000
--           | otherwise  = 50000

-- fmax :: Num a => Ord a => Maybe a -> Maybe a -> Maybe a
-- Just i  `fmax` Just j  = Just i `max` Just j
-- Just i  `fmax` Nothing = Just i
-- Nothing `fmax` Just i  = Just i
-- Nothing `fmax` Nothing = Nothing

-- filterHoken :: HokenKey -> [(HokenKey, Rational)] -> Maybe (HokenKey, Rational)
-- filterHoken key alist = case filter ((==) key . fst) alist of
--                     []  -> Nothing
--                     [x] -> Just x
--                     _   -> Nothing

-- whichLarge :: Hoken -> [(HokenKey, Rational)] -> (Hoken, Rational)
-- whichLarge h alist = (h, fee)
--   where toFee kind = snd <$> filterHoken (h, kind) alist
--         fee = case toFee New `fmax` toFee Old of
--                 Just x  -> x
--                 Nothing -> 0

-- sumHoken :: [HokenCell] -> [(Hoken, Rational)]
-- sumHoken = makeTotallySum .
--            map kojoCalc .
--            Map.assocs .
--            makeSumMap ((&&&) fst3 snd3) thd3
--   where makeTotallySum x = map (flip whichLarge x) ([minBound..maxBound] :: [Hoken])

-- totalHoken :: [HokenCell] -> Int
-- totalHoken = truncate .
--              (`min` 120000) .
--              sum . map snd . sumHoken

fst3 :: (a, b, c) -> a
snd3 :: (a, b, c) -> b
thd3 :: (a, b, c) -> c
fst3 (a, _, _) = a
snd3 (_, a, _) = a
thd3 (_, _, a) = a

combi :: [t] -> [[t]]
combi [] = []
combi [x] = [[], [x]]
combi (x:y) = (++) <$> [[], [x]] <*> combi y

-- x :: [HokenCell]
-- x = [ (Ordinary, New, 20000)
--     , (Ordinary, Old, 40000)
--     , (Ordinary, New, 62000)
--     , (Nenkin, Old, 50000)
--     , (Nenkin, New, 60000)]

listDirectory :: FilePath -> IO [FilePath]
listDirectory fp = do
  contents <- getDirectoryContents fp
  return $ map (fp <>) $ filter (`notElem` [".", ".."]) contents
{-# INLINE listDirectory #-}
-- pathSearch :: Bool -> FilePath -> IO [FilePath]
-- pathSearch inclF fp = do
--   p <- doesDirectoryExist fp
--   case (p, inclF) of
--     (False, True)  -> return [fp]
--     (False, False) -> return []
--     (True, _)      -> do contents <- listDirectory (fp <> "/")
--                          alls     <- mapM (pathSearch inclF) contents
--                          return $ ([fp] <>) $ mconcat alls


-- pSearch :: FilePath -> (String -> IO [FilePath]) -> IO [FilePath]
-- pSearch fp f = do
--   p <- doesDirectoryExist fp
--   if p
--     then do contents <- listDirectory (fp <> "/") -- Directory
--             alls     <- toDiffList <$> mapM (flip pSearch f) contents
--             return $ fp : (mconcat $ fromDiffList alls)
--             -- let toDL s = map toDiffList <$> pSearch s f
--             -- descend  <- concat <$> mapM toDL contents
--             -- let alls = toDiffList fp : descend
--             -- return $ fromDiffList alls
--     else f fp                   -- File

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance (Show a) => Show (DiffList a) where
  show x = "fromList " <> show (fromDiffList x)

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- pSearchD :: FilePath -> IO [FilePath]
-- pSearchD fp = do
--   dirs <- newIORef (mempty :: [FilePath])

--   let loop f = do
--         p <- doesDirectoryExist f
--         when p $ do
--           contents <- listDirectoryFull (f <> "/")
--           forM_ contents $ \element -> do
--             descend <- loop element
--             modifyIORef dirs (descend ++)
--   _    <- loop fp
--   diff <- readIORef dirs
--   return diff

-- xSearchD :: FilePath -> IO [FilePath]
-- xSearchD fp = (`execStateT` []) $ do
--   loop :: FilePath -> StateT [FilePath] IO ()
--   let loop f = do
--         p <- liftIO $ doesDirectoryExist f
--         when p $ do
--           contents <- liftIO $ listDirectory (fp <> "/")
--           forM_ contents $ \element -> do
--             descend <- loop element
--             modify (descend ++)
--   loop fp
-- listDirectoryFull fp = map ((fp ++) . ("/" ++)) <$> listDirectory fp

pathSearchFile :: FilePath -> IO [FilePath]
pathSearchFile fp = do
  diff <- (`execStateT` mempty) $ do
    directoryP <- liftIO $ doesDirectoryExist fp
    if directoryP
      then do contents <- liftIO $ listDirectory (fp ++ "/")
              alls     <- mapM (liftIO . async . pathSearchFile) contents
              let toDL a = wait a >>= (return . toDiffList)
              promise  <- liftIO (mconcat <$> mapM toDL alls)
              modify (promise <>)
      else modify (toDiffList [fp] <>)
  return $ fromDiffList diff

pathSearchDirectory :: FilePath -> IO [FilePath]
pathSearchDirectory fp = (`execStateT` []) $ do
  directoryP <- lift $ doesDirectoryExist fp
  when directoryP $ do
    contents <- lift $ listDirectory (fp ++ "/")
    alls     <- mconcat <$> mapM (lift . pathSearchDirectory) contents
    modify (([fp] ++ alls) ++)

pSearch :: FilePath -> IO [FilePath]
pSearch fp = do
  diff <- do
    dirs <- newIORef (mempty :: DiffList FilePath)
    p    <- doesDirectoryExist fp
    if p
      then do contents <- listDirectory (fp <> "/")
              forM_ contents $ \element -> do
                descend <- pSearch element
                modifyIORef dirs (toDiffList descend <>)
      else modifyIORef dirs (toDiffList [fp] <>)
    readIORef dirs
  return $ fromDiffList diff

pSearchSource :: FilePath -> Source IO FilePath
pSearchSource fp = do
  p <- liftIO $ doesDirectoryExist fp
  if p
    then do contents <- liftIO $ listDirectory (fp <> "/")
            forM_ contents pSearchSource
    else yield fp

dSearch :: FilePath -> IO [FilePath]
dSearch fp = do
  diff <- do
    dirs <- newIORef (mempty :: DiffList FilePath)
    p    <- doesDirectoryExist fp
    if p
      then do modifyIORef dirs (toDiffList [fp] <>)
              contents <- listDirectory (fp <> "/")
              forM_ contents $ \element -> do
                descend <- dSearch element
                modifyIORef dirs (toDiffList descend <>)
      else return ()
    readIORef dirs
  return $ fromDiffList diff

data Conncet v = Connect (v -> v -> v)
data Key a k = Key (a -> k)
data Value a v = Value (a -> v)

mapGenerate :: MakeMap t t1 t2 -> [t] -> Map.Map t1 t2
mapGenerate (MakeCountMap (Key k)) =
  let insert'' mp el = Map.insertWith (+) (k el) 1 mp
  in foldl' insert'' Map.empty
mapGenerate (MakeSingletonMap (Key k) (Value v)) =
  Map.fromList . parMap rseq (k &&& v)
mapGenerate (MakeMonoidMap (Key k) (Value v)) =
  let insert'' mp el =
        Map.insertWith mappend (k el) (v el) mp
  in foldl' insert'' Map.empty
mapGenerate (MakeListMap (Key k) (Value v)) =
  let insert'' mp el =
        let v' = case k el `Map.lookup` mp of
                  Just ans -> (v el) : ans
                  Nothing  -> [v el]
        in Map.insert (k el) v' mp
  in foldl' insert'' Map.empty
mapGenerate (MakeDiffListMap (Key k) (Value v)) =
  let insert'' mp el =
        let v' = case k el `Map.lookup` mp of
                  Just ans -> ans <> toDiffList [v el]
                  Nothing  -> toDiffList [v el]
        in Map.insert (k el) v' mp
  in foldl' insert'' Map.empty

mapGenerateC :: MakeMap t t1 t2 -> [t] -> Map.Map t1 t2
mapGenerateC (MakeListMap (Key k) (Value v)) targetList =
  let insert'' mp el =
        let v' = case k el `Map.lookup` mp of
                  Just ans -> (v el) : ans
                  Nothing  -> [v el]
        in Map.insert (k el) v' mp
  in runConduitPure
    $ CL.sourceList targetList
    .| CL.fold insert'' Map.empty

mapGenerateC (MakeMonoidMap (Key k) (Value v)) targetList =
  let insert'' mp el =
        Map.insertWith mappend (k el) (v el) mp
  in runConduitPure
    $ CL.sourceList targetList
    .| CL.fold insert'' Map.empty

mapGenerateM :: MonadIO m => MakeMap t t1 t2 -> m [t] -> m (Map.Map t1 t2)
mapGenerateM mm t = mapGenerate mm <$> t

(==>) :: [t] -> MakeMap t t1 t2 -> Map.Map t1 t2
(==>) = flip mapGenerate
infixr 1 ==>

(===>) :: MonadIO m => m [t] -> MakeMap t t1 t2 -> m (Map.Map t1 t2)
(===>) = flip mapGenerateM
infixr 1 ===>

data MakeMap a k v where
  MakeMonoidMap ::
    (Ord k , Monoid v) => (Key a k) -> (Value a v) -> MakeMap a k v
  MakeListMap ::
    (Ord k) => (Key a k) -> (Value a v) -> MakeMap a k [v]
  MakeDiffListMap ::
    (Ord k) => (Key a k) -> (Value a v) -> MakeMap a k (DiffList v)
  MakeSingletonMap ::
    (Ord k) => (Key a k) -> (Value a v) -> MakeMap a k v
  MakeCountMap ::
    (Ord k, Integral v) => (Key a k) -> MakeMap a k v

(<<>>) :: (Ord k , Monoid v) => (Key a k) -> (Value a v) -> MakeMap a k v
(<<>>) = MakeMonoidMap
infixl 9 <<>>

(<@@>) :: (Ord k) => (Key a k) -> (Value a v) -> MakeMap a k [v]
(<@@>) = MakeListMap
infixl 9 <@@>

-- mm :: MakeMap Integer Integer (Sum Integer)
-- mm = Key (`mod` 3) `MakeMonoidMap` Value (Sum)

-- mm2 :: MakeMap Integer Integer (V.Vector Integer)
-- mm2 = Key (`mod` 3) `MakeMonoidMap` Value V.singleton

class IsString a => ToCSV a where
  concatenate :: a -> [a] -> a
  _separator :: a
  toCSV :: [a] -> a

  _separator = ","
  toCSV = concatenate _separator

instance ToCSV String where
  concatenate = intercalate

instance ToCSV Tx.Text where
  concatenate = Tx.intercalate
