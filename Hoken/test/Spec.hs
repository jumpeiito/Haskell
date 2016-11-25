{-# LANGUAGE OverloadedStrings #-}

import           Util                   ((++++)
                                        , locEncoding
                                        , makeMap
                                        , scan
                                        , runFile
                                        , ketaNum
                                        , FileSystem (..)
                                        , latexCom
                                        , makeSingleMap)
import           Util.Strdt             (getWeekDateString, strdt, toDay)
import qualified Util.Telephone         as Tel
import           Hoken.Base             (runXdoc, Person (..), config, MeiboMap)
import           Hoken.Parser           (pobjectParse, splitAddress)
import           Hoken.Meibo            (toLatex, toString, toDebug)
import           Hoken.Secrets          ((<<|>>))
import qualified Hoken.Secrets          as Sec -- SecretPerson (..), secretMap, toSecretPerson, (<<|>>)
import qualified Meibo.Base             as MB
import           Data.Time
import           Data.Monoid
import           Data.Maybe             (fromMaybe, isJust)
import           Data.List              (isPrefixOf, intercalate, find)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           Data.Yaml              hiding (Parser, Array)
import           Control.Monad.Reader
import           Text.Parsec            hiding (Line, State)
import           Test.Hspec
import qualified System.IO              as I
import qualified Options.Applicative    as O

testcase1 = "6醍50101伊東090-1901-0111＊4120041200 82400"
testcase2 = "6醍50102伊東090-1901-0111＊2000020000 82400"
testcase3 = "6醍50103伊東090-1901-0111＊2000020000 82400"
testp1 = either (const PersonError) id $ parse pobjectParse "" testcase1
testp2 = either (const PersonError) id $ parse pobjectParse "" testcase2
testp3 = either (const PersonError) id $ parse pobjectParse "" testcase3

testgen = map Sec.toSecretPerson [ ["50101", "Carlo", "5010000", "KyotoCity", "MinamiWard"]
                                 , ["50103", "", "5010000", "KyotoCity", "MinamiWard"]]
testsmp = makeSingleMap Sec.number id testgen

testLine = MB.Line { MB.bunkai = "点在"
                   , MB.bknum  = "50"
                   , MB.han    = ""
                   , MB.kind   = ""
                   , MB.hancho = Just ""
                   , MB.gen    = ""
                   , MB.name   = "7777"
                   , MB.nameP  = ("", "")
                   , MB.ad     = "OsakaCity3-2MinamiOsaka"
                   , MB.tel    = [Tel.Mobile "090-1901-0111"]
                   , MB.work   = ""
                   , MB.exp    = ""
                   , MB.furi   = ""
                   , MB.birthS = ""
                   , MB.birth  = Nothing
                   , MB.year   = Nothing
                   , MB.postal = "6000000"}
testmp = makeMap MB.bunkai id [testLine]           

toLatexSpec :: Spec
toLatexSpec = do
  describe "toLatex" $ do
    it "test1" $ testp1 `toLatex` testsmp `shouldBe` "\\Joseki{Carlo}{82,400}{41,200}"
    it "test2" $ testp2 `toLatex` testsmp `shouldBe` "\\Joseki{伊東}{40,000}{20,000}"
    it "test3" $ testp3 `toLatex` testsmp `shouldBe` "\\Joseki{伊東}{40,000}{20,000}"

toStringSpec :: Spec
toStringSpec = do
  describe "toString" $ do
    -- post -> ad1 -> ad2 -> name
    it "test1" $ toString testp1 testmp testsmp `shouldBe` "\\personallabel{501-0000}{KyotoCity}{MinamiWard}{Carlo}"
    it "test2" $ toString testp2 testmp testsmp `shouldBe` "\\personallabel{600-0000}{OsakaCity3-2}{MinamiOsaka}{伊東}"

toSecretPersonSpec :: Spec
toSecretPersonSpec = do
  describe "toSecretPerson" $ do
    let sp1 = Sec.toSecretPerson ["00000", "京建太郎", "6011111", "京都市南区西九条豊田町3", "京建労会館3F"]
    it "test1" $ Sec.number sp1 `shouldBe` "00000"
    it "test2" $ Sec.name sp1   `shouldBe` "京建太郎"
    it "test3" $ Sec.post sp1   `shouldBe` "6011111"
    it "test4" $ Sec.ad1 sp1    `shouldBe` "京都市南区西九条豊田町3"
    it "test5" $ Sec.ad2 sp1    `shouldBe` "京建労会館3F"
    let sp1 = Sec.toSecretPerson $ replicate 5 ""
    it "test6"  $ Sec.number sp1 `shouldBe` ""
    it "test7"  $ Sec.name sp1   `shouldBe` ""
    it "test8"  $ Sec.post sp1   `shouldBe` ""
    it "test9"  $ Sec.ad1 sp1    `shouldBe` ""
    it "test10" $ Sec.ad2 sp1    `shouldBe` ""
    it "test6" $ Sec.toSecretPerson [] `shouldBe` Sec.SPError
    it "test7" $ Sec.toSecretPerson (replicate 4 "") `shouldBe` Sec.SPError
    it "test7" $ Sec.toSecretPerson (replicate 6 "") `shouldBe` Sec.SPError
    it "test7" $ Sec.toSecretPerson (replicate 2 "") `shouldBe` Sec.SPError

alternativeStringSpec :: Spec
alternativeStringSpec = do
  describe "(<<|>>)" $ do
    it "test1" $ (Just "" <<|>> Just "9")  `shouldBe` "9"
    it "test2" $ (Just "8" <<|>> Just "9") `shouldBe` "8"
    it "test3" $ (Just "8" <<|>> Nothing)  `shouldBe` "8"
    it "test4" $ (Just "" <<|>> Nothing)   `shouldBe` ""
    it "test5" $ (Nothing <<|>> Just "9")  `shouldBe` "9"
    it "test6" $ (Nothing <<|>> Just "")   `shouldBe` ""
    it "test7" $ (Nothing <<|>> Nothing)   `shouldBe` ""

main :: IO ()
main = do
  hspec toLatexSpec
  hspec toStringSpec
  hspec toSecretPersonSpec
  hspec alternativeStringSpec
  return ()
