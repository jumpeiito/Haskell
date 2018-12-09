{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
import Test.DocTest

main :: IO ()
main = doctest ["app/Ukyo.hs", "src/Match/Kumiai.hs"]
