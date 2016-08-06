module KanParse (kanParse) where

import qualified Data.Map       as M
import Data.Maybe               (fromMaybe)
import Data.Monoid
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State

data KP = Keta Integer
  | Num     Integer
  | BigKeta Integer
  | Ksum    (Integer, Integer, Integer)
  deriving (Show, Eq, Ord)

instance Monoid KP where
  mempty = Ksum (0, 0, 0)
  Ksum (x, y, z) `mappend` Num i
    | x == 0    = Ksum (i, y, z)
    | otherwise = Ksum (10 * x + i, y, z)
  Ksum (x, y, z) `mappend` Keta i         = Ksum (0, (if x == 0 then i else i * x) + y, z)
  Ksum (x, y, z) `mappend` BigKeta b      = Ksum (0, 0, (x + y) * b + z)
  Ksum (a, b, c) `mappend` Ksum (x, y, z) = Ksum (a+x, b+y, c+z)
  _              `mappend` _              = mempty

kanjiNumMap, kanjiKetaMap :: M.Map Char Integer
kanjiNumMap = M.fromList [('一', 1), ('二', 2), ('三', 3), ('四', 4),
                          ('五', 5), ('六', 6), ('七', 7), ('八', 8),
                          ('九', 9), ('〇', 0)]

kanjiKetaMap = M.fromList [('十', 10),
                           ('百', 100),
                           ('千', 1000)]

kanjiBigKetaMap = M.fromList [('万', 10000),
                              ('億', 100000000),
                              ('兆', 1000000000000)]

kanjiParseBuilder :: (M.Map Char Integer) -> (Integer -> KP) -> Parser KP
kanjiParseBuilder mp f = do
  c <- oneOf $ M.keys mp
  let n = fromMaybe 1 (M.lookup c mp)
  return $ f n

numParse, ketaParse, bigKetaParse :: Parser KP
numParse     = kanjiParseBuilder kanjiNumMap Num
ketaParse    = kanjiParseBuilder kanjiKetaMap Keta
bigKetaParse = kanjiParseBuilder kanjiBigKetaMap BigKeta
  
kParse :: Parser [KP]
kParse = many $ choice [try numParse, try ketaParse, bigKetaParse]

kpSum :: [KP] -> Integer
kpSum xk = x1 + x2 + x3
  where Ksum (x1, x2, x3) = foldl (<>) mempty xk

kanParse :: Parser Integer
kanParse = kpSum <$> kParse
