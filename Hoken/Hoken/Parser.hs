module Hoken.Parser ( pobjectParse
                    , splitAddress) where

import Hoken.Base               (Person (..))
import Util.Telephone           (telFuncPure, Telephone (..))
import Control.Monad.State
import Text.Parsec              hiding (Line, State)
import Text.Parsec.String
import Test.Hspec

telStringParse :: Parser (Maybe Telephone)
telStringParse = Just <$> telFuncPure
  <|> try (string " " >> return Nothing)
  <|> (string "" >> return Nothing)

hokenParse :: Parser String
hokenParse = do
  year'  <- count 1 digit
  shibu  <- string "醍"
  bunkai <- choice $ map (try . string) ["01", "02", "03", "04", "05", "50"]
  identy <- count 3 digit
  return $ year' ++ shibu ++ bunkai ++ identy

hokenFeeParse :: Parser String
hokenFeeParse = 
  try (string "00000")
  <|> try (string "0000")
  <|> try (string "000")
  <|> try (string "00")
  <|> try ((:) <$> digit <*> hokenFeeParse)

hokenFeeMany :: Parser String
hokenFeeMany = concat <$> many1 hokenFeeParse

testcase = "6醍01001京建太郎075-572-4949＊22000 22000*****6醍50101京花子090-1901-0111＊4120041200 82400"
testcase2 = "6醍50101伊東090-1901-0111＊4120041200 82400"
testcase3 = "6醍01001京建次郎 ＊22000 220006醍50101京花子090-1901-0111＊4120041200 82400" 

makeObjectSpec :: Spec
makeObjectSpec = 
  describe "pobjectParse running test" $ do
    let Right xs = parse pobjectParse "" testcase2
    it "hoken" $ number xs `shouldBe` "50101"
    it "name"  $ name  xs `shouldBe` "伊東"
    it "tel"   $ phone xs `shouldBe` Just (Mobile "090-1901-0111")

_feeSplit :: String -> State (String, [String]) ()
_feeSplit str = do
  forM_ str $ \char -> do
    (fee, returner) <- get
    case (take 2 fee, char /= '0') of
      ("00", True) -> put ([char], reverse fee : returner)
      _            -> put (char:fee, returner)
  (fee, returner) <- get
  put ("", reverse $ reverse fee : returner)

feeSplit :: String -> [Int]
feeSplit str = map read $ snd (_feeSplit str `execState` ("", []))

numToBunkai :: String -> String
numToBunkai "01" = "石田"
numToBunkai "02" = "日野"
numToBunkai "03" = "小栗栖"
numToBunkai "04" = "一言寺"
numToBunkai "05" = "三宝院"
numToBunkai "50" = "点在"
numToBunkai _    = error "illegal argument"

pobjectParse :: Parser Person
pobjectParse = do
  num    <- hokenParse
  name'  <- many1 (noneOf "脱0123456789 ＊")
  tel    <- telStringParse
  _      <- many (char '＊')
  fee    <- hokenFeeMany <* string " "
  _      <- hokenFeeParse
  let feel = feeSplit fee
  let num' = drop 2 num
  return P { number = num'
           , bunkai = numToBunkai $ take 2 num'
           , name   = name'
           , phone  = tel
           , feeStr = fee
           , feeSum = sum feel
           , feeList = feel}

----------------------------------------------------------------------------------------------------
takeWhileP f = (:) <$> f <*> takeWhileP f
  <|> return []

splitAddressParser :: Parser (String, String)
splitAddressParser = do
  let num = "0123456789-"
  addr  <- takeWhileP (noneOf num)
  numb  <- takeWhileP (oneOf num)
  other <- try (many1 anyChar) <|> (eof >> return "")
  return (addr ++ numb, other)

splitAddress :: String -> (String, String)
splitAddress ad = case parse splitAddressParser "" ad of
                    Right x -> x
                    Left _  -> (ad, "")

-- telStringParseSpec :: Spec
-- telStringParseSpec = do
--   describe "telStringParse" $ do
--     it "matches 0xx-xxx-xxxx (fixed) style." $
--       parse telStringParse "" "075-572-4949" `shouldBe` Right "075-572-4949"
--     it "matches 0xxx-xx-xxxx (fixed) style." $
--       parse telStringParse "" "0774-22-2222" `shouldBe` Right "0774-22-2222"
--     it "matches xxx-xxxx (fixed) style." $
--       parse telStringParse "" "572-4949" `shouldBe` Right "572-4949"
--     it "matches 090-xxxx-xxxx (mobile) style." $
--       parse telStringParse "" "090-0000-0000" `shouldBe` Right "090-0000-0000"
--     it "matches 080-xxxx-xxxx (mobile) style." $
--       parse telStringParse "" "080-9999-9999" `shouldBe` Right "080-9999-9999"
--     it "matches 090-xxxxxxxx (mobile) style." $
--       parse telStringParse "" "090-00000000" `shouldBe` Right "090-00000000"
--     it "matches a space character." $
--       parse telStringParse "" " " `shouldBe` Right " "
--     it "matches digit chars when surrounded with non-numeric chars." $
--       parse telStringParse "" "hoge080-9999-9999foo" `shouldBe` Right "080-9999-9999"
--     it "matches digit chars when surrounded with non-numeric chars." $
--       parse telStringParse "" "buz/080-9999-9999-soo" `shouldBe` Right "080-9999-9999"
-- test = do
--   meibo <- Meibo.meiboMain "全" 
--   let mmap = makeMap Meibo.bunkai id meibo
--   case parse pobjectParse "" testcase2 of
--     Right x -> print $ toMeiboData x mmap
--     Left _  -> return ()
