{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Util.Address where

import           Control.Applicative ((<|>))
import           Control.Lens ((^.))
import           Control.Monad (forM_)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Extensible
import           Data.Monoid       ((<>))
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx
import           Data.Attoparsec.Text
import qualified System.IO as I
import           Util.ZenkakuHankaku

type Address = Record
  '[ "town"      >: Tx.Text
   , "apartment" >: Tx.Text ]

oneOf s  = satisfy (`elem` s)
noneOf s = satisfy (`notElem` s)

numbers :: String
numbers = ['0'..'9'] ++ ['０'..'９']

exNumbers :: String
exNumbers = numbers <> "-ー－"

onlyNumbers :: Parser String
onlyNumbers = many1 $ oneOf exNumbers

banchi :: Parser Tx.Text
banchi = string "番地の" <|> string "番地"

chome :: Parser String
chome = (<> numbers) <$> (Tx.unpack <$> string "丁目")

includeBanchi1 :: Parser String
includeBanchi1 = do
  n1 <- onlyNumbers
  b  <- Tx.unpack <$> banchi
  n2 <- onlyNumbers
  return $ n1 ++ b ++ n2

includeBanchi2 :: Parser String
includeBanchi2 = do
  n1 <- onlyNumbers
  b  <- Tx.unpack <$> banchi
  return $ n1 ++ b

includeBanchi3 :: Parser String
includeBanchi3 = do
  n1 <- onlyNumbers
  b1 <- Tx.unpack <$> string "番"
  n2 <- onlyNumbers
  b2 <- Tx.unpack <$> string "号"
  return $ n1 <> b1 <> n2 <> b2

includeBanchi = includeBanchi1
                <|> includeBanchi2
                <|> includeBanchi3

exceptNumbers = many1 $ noneOf numbers

parseAddress = do
  bef <- exceptNumbers
  cho <- option "" chome
  aft <- option "" (includeBanchi <|> onlyNumbers)
  return $ bef <> cho <> aft

makeTypeAddress :: Tx.Text -> Address
makeTypeAddress t = case parse parseAddress t `feed` mempty of
                      Done rest suc -> 
		           #town @= Tx.pack suc
			<: #apartment @= rest
			<: nil
		      _ -> 
		           #town @= t
			<: #apartment @= mempty
			<: nil

test = do
  t <- testData
  forM_ t $ \d -> do
    let ad = makeTypeAddress d
    Tx.putStrLn $ d <> "," <> ad ^. #town

testData = do
  I.withFile "c:/Users/Jumpei/Haskell/Util/Util/test.csv" I.ReadMode $ \handle -> do
    -- enc <- I.mkTextEncoding "cp932"
    I.hSetEncoding handle I.utf8
    Tx.lines <$> Tx.hGetContents handle
