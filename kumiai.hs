import Util
import Strdt
import Data.Time
import Data.List
import Data.Either
import Data.Maybe
import Control.Applicative hiding ((<|>))
import System.Environment
import Text.ParserCombinators.Parsec

data Gender = M | F | Student deriving (Show, Eq, Read)
data KumiaiType = Ippan
                  | Seinen
                  | Minarai
                  | Rourei
                  deriving (Show, Eq, Read)

data Person = H { gender   :: Gender,
                  birthday :: Maybe Day,
                  old      :: Integer,
                  ktype    :: KumiaiType,
                  family   :: [Person],
                  income   :: Maybe Int
                }
            | K { gender   :: Gender,
                  birthday :: Maybe Day,
                  old      :: Integer
                } deriving (Show, Eq, Read)

hokenryoPersonal :: Person -> Int
hokenryoPersonal (H _ _ old' _ f _)
  | old' <= 20 =  7500
  | old' <= 25 =  8500
  | old' == 26 = 10500
  | old' == 27 = 12000
  | old' == 28 = 14000
  | old' == 29 = 16500
  | old' <= 39 = 18500
  | old' <= 64 = 21700
  | old' <= 69 = 19000
  | old' <= 74 && null f = 12000
  | otherwise  = 16500
hokenryoPersonal (K g _ old')
  | g == Student = 14500
  | old' <= 11   =  4500
  | old' <= 39   =  5000
  | old' <= 64   =  7400
  | otherwise    =  5500

hokenryo :: Person -> Int
hokenryo pso = psoH + psoK
  where psoH = hokenryoPersonal pso
        psoK = sum $ take 5 $ sortBy (flip compare) (map hokenryoPersonal $ family pso)

shakaiHokenryo :: Person -> Maybe Float
shakaiHokenryo pso = do
  income' <- income pso
  let old'  = old pso
      ratio = if old' >= 40 && old' <= 64 then 0.1021 else 0.1160
  return $ (realToFrac income') * ratio

toInt :: [Char] -> Maybe Int
toInt n
  | n == ""   = Nothing
  | otherwise = Just $ read n

toGender :: String -> Gender
toGender n = read n

oldCalc :: String -> Day -> (Maybe Day, Integer)
oldCalc birth' start = (b', old')
  where b'   = strdt birth'
        old' = fromJust $ (flip howOld) start <$> b'

hParse :: Day -> Parser Person
hParse start = do
  b' <- count 8 digit
  g' <- char ',' *> oneOf "MF"
  i' <- char ',' *> try (many1 digit) <|> string ""
  let (birth', old') = oldCalc b' start
  return $ H { gender   = toGender [g'],
               birthday = birth',
               ktype    = Ippan,
               old      = old',
               income   = toInt i',
               family   = [] }

kParse :: Day -> Parser Person
kParse start = do
  b' <- count 8 digit
  char ','
  g' <- try (string "M") <|>
        try (string "F") <|>
        try (string "Student")
  let (birth', old') = oldCalc b' start
  return $ K { gender   = toGender g',
               birthday = birth',
               old      = old' }

personParse :: Day -> String -> Either ParseError Person
personParse start s' = do
  h':k' <- return $ split '/' s'
  let k = rights $ map (parse (kParse start) "") k'
  h <- parse (hParse start) "" h'
  return $ h { family = k }
  
currentTimeYear :: UTCTime -> Day
currentTimeYear u =
  fromGregorian (toYear $ utctDay u) 4 2

main :: IO ()
main = do
  [parsee]  <- getArgs
  start <- currentTimeYear <$> getCurrentTime
  case personParse start parsee of
    Right x -> putStrLn . show . hokenryo $ x
    Left _  -> putStrLn "Illegal input"

tstr1 = "19450623,M,400000/19450714,F/19800714,Student/20100304,M/20140101,F"
-- tstr2 = "19800714,M/"
-- tstr3 = "19850723,M,400000/19850627,F/20070127,M/20101208,F" -- 30,500
-- tstr4 = "19470927,M,0/19440905,F" -- 24,500
