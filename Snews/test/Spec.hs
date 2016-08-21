import Data.Time
import Test.Hspec
import Test.QuickCheck
import System.IO
import Snews.OrgParse
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

leapYearSpec :: Spec
leapYearSpec = do
  describe "leapYear" $ do
    it "zero"  $ leapYear 0    `shouldBe` False
    it "case1" $ leapYear 1901 `shouldBe` False
    it "case2" $ leapYear 2000 `shouldBe` True
    it "case3" $ leapYear 1900 `shouldBe` False
    it "case4" $ leapYear 2016 `shouldBe` True
    it "case5" $ leapYear 2015 `shouldBe` False
    it "case6" $ leapYear 2014 `shouldBe` False
    it "case7" $ leapYear 1800 `shouldBe` False
    it "case8" $ leapYear 1700 `shouldBe` False
    it "case9" $ leapYear 1600 `shouldBe` True

makeMonthListSpec :: Spec
makeMonthListSpec = do
  let f = fromGregorian
  describe "makeMonthList" $ do
    it "case1" $ makeMonthList (f 2016 7 10) 2016 7 `shouldBe` [1..10]
    it "case2" $ makeMonthList (f 2012 12 31) 2016 7 `shouldBe` [1..31]
    it "case3" $ makeMonthList (f 2016 7 10) 2012 3 `shouldBe` [1..31]
    it "case4" $ makeMonthList (f 2016 7 10) 2012 2 `shouldBe` [1..29]
    it "case5" $ makeMonthList (f 2016 2 29) 2016 2 `shouldBe` [1..29]
    it "case6" $ makeMonthList (f 2016 2 28) 2016 2 `shouldBe` [1..28]
    it "case7" $ makeMonthList (f 2015 2 28) 2016 2 `shouldBe` [1..29]

main :: IO ()
main = hspec $ do
  leapYearSpec
  makeMonthListSpec
