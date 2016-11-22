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
import qualified Hoken.Secrets          as Sec
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

main :: IO ()
main = do
  hspec toLatexSpec
  hspec toStringSpec
  return ()
