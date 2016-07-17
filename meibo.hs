{-# LANGUAGE FlexibleContexts #-}

import Util
import Strdt
import Telephone

import Data.Time
import Data.List
import Data.Maybe
-- import Data.Monoid
import Control.Applicative  hiding (many, (<|>))
import qualified Control.Monad.State as MS
import Control.Monad.Writer
import Text.Parsec          hiding (Line)
import Text.Parsec.String
import System.Process
import System.Environment
import qualified Data.Map   as Map
import qualified System.IO  as I

-- file = ".test"

data Line s = Line { bunkai :: String,
                     bknum  :: String,
                     han    :: String,
                     kind   :: String,
                     hancho :: Maybe String,
                     gen    :: String,
                     name   :: String,
                     nameP  :: (String, String),
                     ad     :: String,
                     tel    :: [Telephone],
                     work   :: String,
                     exp    :: String,
                     furi   :: String,
                     birthS :: String,
                     birth  :: Maybe Day,
                     year   :: Maybe Integer
                   } deriving (Show, Eq)

data Key =
  Bunkai String
  | Bk String
  | Han String
  | K String
  | Name String
  | Address String
  | Ftel String
  | Mtel String
  | Work String
  | Year String
  | Old (Maybe Day, Maybe Day)
  | Or  [Key]
  | And [Key]
  | Not Key deriving (Show, Eq, Read)

test :: Day -> Parser (Line s)
test day = do
  bnk   <- choice [string "石田", string "日野", string "小栗栖",
                   string "一言寺", string "三宝院", string "点在"] <* sep'
  hn    <- many1 digit <* sep'
  sym   <- cell <* sep'
  hcho  <- cell <* sep'
  nm    <- cell <* sep'
  ad'   <- cell <* sep'
  tel'  <- cell <* sep'
  exp'  <- cell <* sep'
  exp2' <- cell <* sep'
  fu'   <- cell <* sep'
  bir   <- cell
  let adtel  = ad' ++ "・" ++ tel'
  let telp   = telParse adtel
  let birth' = strdt bir :: Maybe Day
  return $ Line { bunkai = bnk,
                  bknum  = bunkaiNumber bnk,
                  han    = hn,
                  kind   = removeSymbol sym,
                  hancho = case hcho of "" -> Nothing; _ -> Just "●",
                  gen    = adtel ++ "\n",
                  name   = nm,
                  nameP  = nameParse nm,
                  ad     = deleteStrMap (map telString telp) adtel,
                  tel    = telp,
                  work   = exp',
                  furi   = fu',
                  birthS = bir,
                  birth  = birth',
                  year   = howOld <$> birth' <*> (Just day),
                  Main.exp = exp2' }
  where sep  = ','
        cell = many (noneOf [sep])
        sep' = char sep
  
removeSymbol :: String -> String
removeSymbol = deleteStrMap ["◎", "○"]

bunkaiNumber :: String -> String
bunkaiNumber s = case s of
  "石田"   -> "01"
  "日野"   -> "02"
  "小栗栖" -> "03"
  "一言寺" -> "04"
  "三宝院" -> "05"
  "点在"   -> "50"
  _        -> ""
----------------------------------------------------------------------------------------------------
_nameParse :: Parser (String, String)
_nameParse = do
  fam <- (many1 $ noneOf "　") <* char '　'
  fir <- many1 $ noneOf "　"
  return $ (fam, fir)

nameParse :: String -> (String, String)
nameParse n = case parse _nameParse "" n of
  Right p -> p
  Left _  -> ("", "")
----------------------------------------------------------------------------------------------------
deleteStr :: String -> String -> String
deleteStr key target = snd $ runWriter (delStr key target)
  where (>>>) x y = drop (length x) y
        delStr :: String -> String -> Writer String String
        delStr _ [] = return []
        delStr key target@(t:ts)
          | key `isPrefixOf` target = delStr key (key >>> target)
          | otherwise               = do { tell [t]; delStr key ts}
  
deleteStrMap :: [String] -> String -> String
deleteStrMap [] s = s
deleteStrMap (x:xs) s = deleteStrMap xs (deleteStr x s)

toL :: String -> [String]
toL = split ','

(<@>) :: String -> Int -> String
(<@>) str n = (toL str)!!n

blankP :: String -> Int -> Bool
blankP s n = (s <@> n) == ""

-- syncF ["a", "b", "c"] ["d", "e", "f"] [2] (++)
-- -> ["a","b","cf"]
syncF :: [a] -> [a] -> [Int] -> (a -> a -> a) -> [a]
syncF a b numList f = snd $ runWriter (syncFr a b 0)
  where syncFr [] _ _ = return []
        syncFr _ [] _ = return []
        syncFr (x:xs) (y:ys) n = do
          (if (n `elem` numList)
           then tell [f x y]
           else tell [x])
          syncFr xs ys (n+1)

lineMerge :: String -> [String] -> [String]
lineMerge str (l:ls) =
  (intercalate "," syn):ls
  where str'     = toL str
        header   = toL l
        plus a b = a ++ "・" ++ b
        syn      = syncF header str' [5,6] plus
        
firstTrans :: [String] -> [String]
firstTrans lines = reverse answer
  where (_, (_, answer)) = MS.runState (fTrans2 lines) ("0", [])

fTrans2 :: [String] -> MS.State (String, [String]) ()
fTrans2 ls = do
  MS.forM ls $ \n -> do
    (num, ret) <- MS.get
    case (n `blankP` 4, n `blankP` 1) of
      (True, _) -> MS.put (num,   n `lineMerge` ret)
      (_, True) -> MS.put (num,   (inner num n):ret)
      (_, _)    -> MS.put (n<@>1, n:ret)
  return ()
  where inner n l = case toL l of
          h:_:r -> intercalate "," $ h:n:r
          _ -> ""

secondTrans :: Day -> [String] -> [Line s]
secondTrans _ [] = []
secondTrans day (x:xs) = case (parse (test day) "" x) of
  Right s -> s:(secondTrans day xs)
  Left _  -> secondTrans day xs

trans day = (secondTrans day) . firstTrans

fixTel, mobileTel :: Line s -> [Telephone]
fixTel = fixFilter . tel
mobileTel = mobileFilter . tel

mobileBlankP :: Line s -> Bool
mobileBlankP = (==[]) . mobileTel

addressMap :: [Line s] -> Map.Map String [Line s]
addressMap = makeMap ad id 

testFilter lines = filter hofoo lines
  where hofoo lyne = case (key lyne, mobileBlankP lyne) of
          (Just _, True) -> True
          _              -> False
        mapp  = addressMap lines
        key l = Map.lookup (ad l) mapp

lineToTel :: [Telephone] -> String
lineToTel = (intercalate "・") . map telString

lineMobile, lineFix :: Line s -> String
lineMobile = lineToTel . mobileTel
lineFix    = lineToTel . fixTel

functionsToString :: [(Line s -> String)] -> Line s -> String
functionsToString fs l = intercalate "," $ map (\f -> f l) fs

mainString :: Line s -> String
mainString l =
  functionsToString [bunkai, han, kind,  name, furi,
                     blank, ad', lineMobile, lineFix, work, Main.exp,
                     (show . birth), (show . year)] l
  where ad' line = deleteStrMap [".", "・", "･", " ", "　"] $ ad line
        blank _           = ""
----------------------------------------------------------------------------------------------------
-- makeTelList :: [Line s] -> [Line s]
-- makeTelList (f:fs) = coref 
----------------------------------------------------------------------------------------------------
hanchoMap :: [Line s] -> Map.Map Int [Line s]
hanchoMap = makeMap f id
  where f line' = (100 * toInt (bknum line')) + (toInt (han line'))
        toInt s = read s :: Int
        
hanchoFilter = filter (isJust . hancho)

safeHead [] = Nothing
safeHead (x:_) = Just x

(-->) :: Maybe t -> (t -> String) -> String
Just s  --> f = f s
Nothing --> _ = ""

hanchoList :: (t, [Line s]) -> [String]
hanchoList (_, v) = [bkn, bnk, hn, name', fam', len]
  where hncho = safeHead $ hanchoFilter v
        name' = hncho --> name
        bnk   = hncho --> bunkai
        bkn   = hncho --> bknum
        hn    = hncho --> han
        fam'  = case nameP <$> hncho of Just (f1, _) -> f1; Nothing -> ""
        len   = show $ length v

hanInfo :: (t, [Line s]) -> String
hanInfo = (intercalate ",") . hanchoList

hanDay :: [String] -> String -> String
hanDay lis bk
  | bk == ""   = ""
  | bk == "50" = ""
  | otherwise  = case dayGen of
    "_" -> ""
    _   -> dayGen
  where bknumber = read bk :: Int
        dayGen   = lis!!(bknumber - 1)

hanOutput :: (Int, [String]) -> (t, [Line s]) -> String
hanOutput (month, day) = opfunc . hanchoList
  where arguments (bn:_:h:_:n:l:_) =
          intercalate "}{" [h,l,n,(show month),(hanDay day bn)]
        arguments _ = ""
        opfunc list = 
          "\\hancholine{" ++ (arguments list) ++ "}"
----------------------------------------------------------------------------------------------------
makeTable :: Integer -> [(String, Day, Day)]
makeTable y' =
  [("見", (fG (y'-21) 4 2), (fG 3000 4 1)),
   ("青", (fG (y'-26) 4 2), (fG (y'-21) 4 1)),
   ("一", (fG (y'-70) 4 2), (fG (y'-26) 4 1)),
   ("老", (fG 1900 4 2), (fG (y'-70) 4 1))]
  where fG = fromGregorian

betweenP :: Ord a => (a, a) -> a -> Bool
betweenP (start, end) key
  | start < key && key < end = True
  | otherwise = False

betweenP2 :: Ord a => Eq a => (a, a) -> a -> Bool
betweenP2 (start, end) key
  | start <= key && key <= end = True
  | otherwise = False

fromBirthday :: [(String, Day, Day)] -> Day -> String
fromBirthday table d = coref table
  where coref [] = ""
        coref ((k, start, end):xs)
          | betweenP2 (start, end) d = k
          | otherwise = coref xs

checK :: [(String, Day, Day)] -> Line s -> Bool
checK table l = (kind l) == (kind2 l)
  where kind2 line = fromJust $ fromBirthday table <$> (birth line)

checkFilter :: [(String, Day, Day)] -> [Line s] -> [Line s]
checkFilter table l = filter (\n -> checK table n == False) l
----------------------------------------------------------------------------------------------------
translateFunc, translateFuncPartial :: (Line s -> String) -> String -> Line s -> Bool
translateFunc func key = (\n -> key == func n) 

translateFuncPartial func key = (\n -> key `isInfixOf` func n)

translateFuncTel :: (Line s -> [Telephone]) -> String -> (Line s -> Bool)
translateFuncTel func key = (\n -> key `isInfixOf` telpn n)
  where telpn = intercalate "," . map telString . func

translateFuncYear :: String -> (Line s -> Bool)
translateFuncYear y =
  (\n -> case year n of
     Nothing -> False
     Just x  -> (read y :: Integer) == x)

translateFuncOld :: (Maybe Day, Maybe Day) -> (Line s -> Bool)
translateFuncOld (start, end) =
  (\n -> fromJust $ (&&) <$> (startBool n) <*> (endBool n))
  where startBool n' = (>=) <$> birth n' <*> start
        endBool n'   = (<=) <$> birth n' <*> end

translateFold :: (Bool -> Bool -> Bool) -> Bool -> [Key] -> (Line s -> Bool)
translateFold func bool list =
  (\line -> foldl func bool $ map (\f -> (translate f) line) list)

translate :: Key -> (Line s -> Bool)
translate (Or  list)  = translateFold (||) False list
translate (And list)  = translateFold (&&) True list
translate (Not term)  = not . (translate term)
translate (Bunkai s)  = translateFunc bunkai s
translate (Bk i)      = translateFunc bknum i
translate (Han i)     = translateFunc han i
translate (K i)       = translateFuncPartial kind i
translate (Name s)    = translateFuncPartial name s
translate (Address s) = translateFuncPartial ad s
translate (Ftel s)    = translateFuncTel fixTel s
translate (Mtel s)    = translateFuncTel mobileTel s
translate (Work s)    = translateFuncPartial work s
translate (Year s)    = translateFuncYear s
translate (Old s)     = translateFuncOld s

seek :: Key -> [Line s] -> [Line s]
seek term = filter (translate term) 
----------------------------------------------------------------------------------------------------
keyParseLine = sepBy keyParseTerm $ char ','

keyParseBuilder (s, e) f op = do
  char s
  exp' <- f
  char e
  return $ op exp'

aChar = noneOf "=,)]>"

keyParseOr, keyParseAnd, keyParseNot, keyParseTermWord, keyParseTerm :: Parser Key

keyParseOr  = keyParseBuilder ('[', ']') keyParseLine Or
keyParseAnd = keyParseBuilder ('(', ')') keyParseLine And
keyParseNot = keyParseBuilder ('<', '>') keyParseTerm Not

bkhanCodeParse :: Parser Key
bkhanCodeParse = do
  bk'  <- count 2 digit
  han' <- many digit
  return $ And [Bk bk', Han han']

yearoldParse :: Parser Key
yearoldParse = do
  let ac = noneOf "=,)]>&"
  start <- (many1 ac) <* char '&'
  end   <- many1 ac
  let s = strdt start :: Maybe Day
  let e = strdt end :: Maybe Day
  return $ Old (s, e)

returnParse parser code' = case parse parser "" code' of
  Right c -> c
  Left _  -> And []

bkhanCode :: String -> Key
bkhanCode = returnParse bkhanCodeParse

yearOld :: String -> Key
yearOld = returnParse yearoldParse

keyParseTermWord = do
  key' <- (many1 aChar) <* char '='
  val' <- many1 aChar
  case key' of
    "bunkai" -> return $ Bunkai val'
    "bk"     -> return $ Bk val'
    "han"    -> return $ Han val'
    "k"      -> return $ K val'
    "name"   -> return $ Name val'
    "ad"     -> return $ Address val'
    "ftel"   -> return $ Ftel val'
    "mtel"   -> return $ Mtel val'
    "work"   -> return $ Work val'
    "year"   -> return $ Year val'
    "old"    -> return $ yearOld val'
    "c"      -> return $ bkhanCode val'
    _        -> return $ And []

keyParseTerm = do
  try keyParseOr
  <|> try keyParseAnd
  <|> try keyParseNot
  <|> try keyParseTermWord

keyParse :: String -> Key
keyParse str = case parse keyParseLine "" str of
  Right x -> head x
  Left _  -> And []

seekS :: String -> [Line s] -> [Line s]
seekS str = seek (keyParse str)
----------------------------------------------------------------------------------------------------
runRuby :: IO (I.Handle, I.Handle, I.Handle, ProcessHandle)
runRuby = runInteractiveProcess "ruby" ["meibo.rb"] Nothing Nothing
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  I.hSetEncoding I.stdout I.utf8
  (y', m', d')    <- today
  (_, sout, _, _) <- runRuby
  let currentDay = fromGregorian y' m' d'
  mainList        <- (trans currentDay) <$> lines <$> I.hGetContents sout
  args            <- getArgs
----------------------------------------------------------------------------------------------------
  case args of
    ["seek", key] -> mainOut $ seekS key mainList
    []            -> mainOut mainList
    ["check"]     -> let table = makeTable y' in (mainOut $ checkFilter table mainList)
    ["hch"]       -> (printer hanInfo) $ mapToList mainList
    "hchp":l      -> (printer (hanOutput (m' , l))) $ mapToList mainList
    -- "test"        -> mainOut $ testFilter mainList
    _             -> putStrLn ""
----------------------------------------------------------------------------------------------------
  where printer f = mapM_ (putStrLn . f)
        mainOut   = (printer mainString)
        mapToList = Map.assocs . hanchoMap

-- data Join a = Join String deriving (Show, Eq)

-- instance Monoid (Join a) where
--   mempty = Join ""
--   Join "" `mappend` (Join y) = Join y
--   (Join x) `mappend` Join "" = Join x
--   (Join x) `mappend` (Join y) = Join $ x ++ "," ++ y

hoge :: MS.State [String] Int
hoge = do
  x <- MS.get
  MS.forM ["hoge", "foo", "buz"] (\n -> MS.modify (++[n]))
  return $ length x
