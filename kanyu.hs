import Util
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map               as M
import Control.Applicative hiding ((<|>))
import qualified System.IO              as I
import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Expr
import Text.Printf
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

alist :: [(String, String)]
alist =
  [("10", "日頃のつきあい"),
   ("11", "親方のすすめ"),
   ("12", "家族のすすめ"),
   ("13", "組合員紹介"),
   ("14", "役所・団体の紹介"),
   ("16", "元組合員"),
   ("17", "宣伝広告"),
   ("18", "その他") ]

thd :: (a, b, c) -> c
thd (x, y, z) = z

collectKinds :: String -> [String]
collectKinds = concat . map (split '+') . lines

count :: Eq a => a -> [a] -> (a, Int)
count key list =
  (key, length $ filter (==key) list)

collection :: [String] -> [(String, String, Int)]
collection list =
  map corefunc alist
  where corefunc (key, title) =
          let (key', len) = Main.count key list in
          (key', title, len)

corefunc :: Int -> (String, String, Int) -> (String, String, Int, Float)
corefunc sum' (key, title, k) =
  (key, title, k, perc)
  where k'   = realToFrac k
        sum'' = realToFrac sum'
        perc = (k'/sum''*100)
        -- perc = Text.Printf.printf "%2.1f%%" (k' / sum'' * 100)

-- collectWithPercent :: [String] -> [(String, String, Int, String)]
collectWithPercent list =
  map (corefunc sum') xlist
  where xlist = collection list
        sum'  = sum $ map thd xlist

puts (num, title, total, perc) =
  Text.Printf.printf "%s, %d, %2.1f%%\n" title total perc

main = do
  cont <- readUTF8File ".test"
  I.hSetEncoding I.stdout I.utf8
  mapM_ puts $ collectWithPercent $ collectKinds cont
