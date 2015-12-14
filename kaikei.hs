import Util
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map               as M
import Control.Applicative hiding ((<|>))
import qualified System.IO              as I
import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data KStream = In | Out deriving (Show, Eq)

data KTypes = KTitle String String
            | KLine String String [Int] deriving (Show)

data KNode   = Single KStream String
             | KRegular Int String
             | Zennen String
             | Total String [KNode]
             | KTest String [KNode]
             deriving (Show)

-- instance Show KTypes where
--   show (KTitle _ s) = s
--   show (KLine q s xs)  = q ++ "," ++ s ++ "," ++ intercalate "," (map show xs)

rest, inRealKaikei, inKaikei, outKaikei :: [KNode]
rest = [ KRegular 0 "現金",
         KRegular 0 "普通預金・２",
         KRegular 0 "仮払金",
         KRegular 5 "預り金"]

inRealKaikei = [ Total "組合費小計" [Single In "支部費収入",
                                     Single In "支部費ＮＯ２",
	                             Single In "新加入分会費収入",
	                             Single In "点在分会費収入" ],
                 Single In "共済還元金収入",
                 Single In "援助金収入（その他）",
                 Single In "雑収入"]

inKaikei    = Zennen "A" : inRealKaikei

outKaikei = [ Total "行動費関係小計" [ Single Out "研修会議費",
                                       Single Out "執行委員会", 
                                       Single Out "三役費", 
                                       Single Out "交通費", 
                                       Single Out "役員手当"], 
               Total "専門部費小計" [ Single Out "組織部費", 
                                       Single Out "賃対部費", 
                                       Single Out "文厚部費", 
                                       Single Out "社保対部費", 
                                       Single Out "税対部費", 
                                       Single Out "教育部費", 
                                       Single Out "宣伝部費", 
                                       Single Out "技住対部費", 
                                       Single Out "自治体部費", 
                                       Single Out "労対部費" ], 
                Single Out "メーデー",
                Single Out "共済無給付支部拠出金",
                Single Out "共同闘争費",
                Single Out "大会費",
                Total "援助金小計" [ Single Out "分会援助金", 
                                     Single Out "青年援助金", 
                                     Single Out "主婦援助金", 
                                     Single Out "シニア援助金"],
                Total "事務所関係小計" [ Single Out "什器備品購入費用", 
                                         Single Out "リース料", 
                                         Single Out "事務用品費", 
                                         Single Out "通信費", 
                                         Single Out "新聞・図書費", 
                                         Single Out "車両維持費", 
                                         Single Out "事務所費", 
                                         Single Out "水道光熱費"],
                Total "積立金小計" [ Single Out "支部事務所積立支出金", 
                                     Single Out "車両，機械積立支出金", 
                                     Single Out "首長選会計積立支出金", 
                                     Single Out "闘争会計支出金", 
                                     Single Out "運動会会計支出金", 
                                     Single Out "特別闘争会計支出金"],
                Total "雑支出小計" [ Single Out "慶弔費", Single Out "雑費"],
                Single Out "アルバイト費",
                Single Out "予備金"]

lexer  = P.makeTokenParser emptyDef
whiteSpace = P.whiteSpace lexer

kaikeiFilter :: [String] ->  [String]
kaikeiFilter = reverse . foldl kTrans []
  where kTrans seed l@(x:' ':_)
          | x `elem` "9ABCDEGIJLORS " = l:seed
          | otherwise                = seed
        kTrans seed _ = seed

strip :: String -> String
strip s = strip' s ""
  where strip' "" seed         = reverse seed
        strip' (x:xs) seed
          | x == ' '  = strip' xs seed
          | otherwise = strip' xs (x:seed)

stringToInt :: String -> Int
stringToInt "" = 0
stringToInt s = read s

number    = many1 digit

klineParseA :: Parser KTypes
klineParseA = do
  s1 <- whiteSpace *> number
  s2 <- whiteSpace *> number
  s3 <- whiteSpace *> number
  name <- many1 $ satisfy (`notElem` "1234567890")
  s4 <- number
  s5 <- whiteSpace *> number
  s6 <- try (whiteSpace *> number) <|> string ""
  return $ KLine "" (strip name) (map stringToInt [s1, s2, s3, s4, s5 ,s6])

klineParseB :: Parser KTypes
klineParseB = do
  s2   <- whiteSpace *> number
  s3   <- whiteSpace *> number
  name <- many1 $ satisfy (`notElem` "1234567890")
  s4   <- number
  s5   <- whiteSpace *> number
  s6   <- whiteSpace *> number
  return $ KLine "" (strip name) (map stringToInt ["", s2, s3, s4, s5 ,s6])

klineTitle :: Parser KTypes
klineTitle = do
  types  <- (:[]) <$> oneOf "9ABCDEGIJLORS"
  s'     <- many1 $ satisfy (`notElem` "1234567890")
  return $ KTitle types (strip s')

klineParse :: Parser KTypes
klineParse = 
  try klineTitle  <|> try klineParseB <|> try klineParseA

kParse :: String -> Either ParseError KTypes
kParse = parse klineParse ""

type KMap = M.Map (String, String) [Int]

kaikeiMap :: [KTypes] -> KMap
kaikeiMap = folder M.empty ""
  where folder map' _ [] = map'
        folder map' k' (x:xs) = 
          case x of
          KTitle kind' _     -> folder map' kind' xs
          KLine _ kind ints  -> folder (M.insert (k', kind) ints map') k' xs


getVal :: Ord a => a -> Int -> M.Map a [b] -> Maybe b
getVal key' nth map' = do
  val <- M.lookup key' map'
  return $ val !! nth

findVal :: KNode -> KMap -> Maybe Int
Zennen s     `findVal` m = getVal (s, "前期繰越収支差額") 5 m
Single In s  `findVal` m = getVal ("A", s) 3 m
Single Out s `findVal` m = negate <$> getVal ("A", s) 2 m
Total _ xs   `findVal` m = return $ sum $ mapMaybe (`findVal` m) xs
KTest s _    `findVal` m = getVal ("A", s) 2 m
KRegular i s `findVal` m
  | i <= 2    = getVal ("A", s) i m
  | otherwise = negate <$> getVal ("A", s) i m

findValStr :: KNode -> KMap -> String
findValStr kn mp =
  case kn `findVal` mp of
  Just x  -> show $ abs x
  Nothing -> "0"

knodeStr :: KNode -> KMap -> String
knodeStr kn mp = coreStr ++ findValStr kn mp
  where trans element = knodeStr element mp ++ "\n"
        coreStr = case kn of
          Zennen _   -> "前期繰越収支差額,"
          Total s xs -> concatMap trans xs ++ s ++ ","
          Single _ s -> s ++ ","

testSum mp = sum . mapMaybe (`findVal` mp)

main :: IO ()
main = do
  let readLines fp = lines <$> readUTF8File fp
      translate    = rights . map kParse . kaikeiFilter
  alist <- translate <$> readLines "kaikei.txt"
  let mp = kaikeiMap alist
  I.hSetEncoding I.stdout I.utf8
  mapM_ (putStrLn . (`knodeStr` mp)) inKaikei
  mapM_ (putStrLn . (`knodeStr` mp)) outKaikei


testMap :: IO KMap
testMap = do 
  let readLines fp = lines <$> readUTF8File fp
      translate    = rights . map kParse . kaikeiFilter
  kaikeiMap <$> translate <$> readLines "kaikei.txt"

testAlist :: IO [KTypes]
testAlist = do
  let readLines fp = lines <$> readUTF8File fp
      translate    = rights . map kParse . kaikeiFilter
  translate <$> readLines "kaikei.txt"
