makeObjectSpec :: Spec
makeObjectSpec = do
  describe "pobjectParse running test" $ do
    let Right xs = parse pobjectParse "" testcase2
    it "hoken" $ number xs `shouldBe` "50101"
    it "name"  $ name  xs `shouldBe` "伊東"
    it "tel"   $ phone xs `shouldBe` Just (Tel.Mobile "090-1901-0111")

main :: IO ()
main = do
  hspec makeObjectSpec
  return ()
