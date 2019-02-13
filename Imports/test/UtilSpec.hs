{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Import
import Util
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P
import Test.Hspec
import Test.Hspec.QuickCheck

plus2 = (+2)

spec :: Spec
-- spec = do
--   describe "plus2" $ do
--     it "basic check" $ plus2 0 `shouldBe` 2
--     it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
--     prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
spec = do
  describe "parser test" $ do
    it "blank1" $ P.parse blank "" "   " `shouldBe` Right "   "
    it "blank2" $ P.parse blank "" "" `shouldBe` Right ""
    it "pragmaParser1" $
      P.parse pragmaParser "" "{-# LANGUAGE NoImplicitPrelude #-}"
        `shouldBe` Right (Pg ["NoImplicitPrelude"] 17)
    it "pragmaParser2" $
      P.parse pragmaParser "" "{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}"
        `shouldBe` Right (Pg ["NoImplicitPrelude", "OverloadedStrings"] 17)
    it "pragmaParser3" $
      P.parse pragmaParser "" "{-# LANGUAGE     NoImplicitPrelude #-}"
        `shouldBe` Right (Pg ["NoImplicitPrelude"] 17)
    it "importNameComponentParser1" $
      isLeft (P.parse importNameComponentParser "" "hoge") `shouldBe` True
    it "importNameComponentParser2" $
      P.parse importNameComponentParser "" "Hoge"
        `shouldBe` Right "Hoge"
    it "importParser1" $
      P.parse importParser "" "import Data.Text"
        `shouldBe` Right (Imp ("Data.Text", Nothing, 9))
    it "importParser2" $
      P.parse importParser "" "import Conduit"
        `shouldBe` Right (Imp ("Conduit", Nothing, 7))
    it "importParser3" $
      P.parse importParser "" "import     Conduit"
        `shouldBe` Right (Imp ("Conduit", Nothing, 7))
    it "importParser4" $
      P.parse importParser "" "import qualified Data.List as L"
        `shouldBe` Right (ImpQ ("Data.List", Just "as L", 9))
    it "importParser5" $
      P.parse importParser "" "import qualified Data.List     as L"
        `shouldBe` Right (ImpQ ("Data.List", Just "as L", 9))
