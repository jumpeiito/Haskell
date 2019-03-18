import Test.DocTest
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

main = doctest ["-isrc", "src/Util.hs"]
