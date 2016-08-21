module Zipcode.ZipFormatta (fmtFold) where

import Text.Parsec
import Text.Parsec.String
import Text.StringLike

data Formatta = FAd | FPt | FS String | Ferror String
  deriving (Show, Eq)

fmtAddress, fmtPostal, fmtOther :: Parser Formatta
fmtAddress = do { _ <- string "{ad}"; return FAd }
fmtPostal  = do { _ <- string "{pt}"; return FPt }
fmtOther   = FS  <$> many1 (noneOf "{}")

strToFormatta :: Parser [Formatta]
strToFormatta = many $ choice [try fmtAddress, try fmtPostal, fmtOther]

fmtFold :: StringLike a => a -> a -> String -> String
fmtFold ad p s = either (const mempty) (foldl classify mempty)
                        $ parse strToFormatta "" s
  where [ad', p'] = map castString [ad, p]
        classify init' FAd    = init' ++ ad'
        classify init' FPt    = init' ++ p'
        classify init' (FS s') = init' ++ s'
        classify init' (Ferror _) = init'
