import Util
import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.String
import qualified System.IO              as I

file = ".test"

data Address   = Office String | Home String deriving (Eq, Show)
data Telephone = Fix [String] | Mobile [String] deriving (Eq, Show)

-- data Line s = Line { bunkai :: String,
--                      han    :: String,
--                      kind   :: String,
--                      name   :: String,
--                      ad     :: Address,
--                      tel    :: Telephone,
--                      work   :: String,
--                      exp    :: String
--                    }



-- main :: IO [String]
-- main = do
--   meiboData <- lines <$> readUTF8File ".test"
--   return meiboData
csvData = endBy line eol
line    = sepBy cell $ char ','
cell    = many $ noneOf ",\n"
eol     = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV src = parse csvData "*ParseError*" src


filt str = leng > 4

  where leng = length $ filter (=='\t') str

main = do
  cont <- filter filt <$> lines <$> readUTF8File ".test"
  I.hSetEncoding I.stdout I.utf8
  mapM_ putStrLn cont
  
