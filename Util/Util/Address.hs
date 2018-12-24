{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Util.Address where

import           Control.Applicative ((<|>))
import           Data.Extensible
import           Data.Monoid       ((<>))
import qualified Data.Text as Tx
import           Data.Attoparsec.Text

type Address = Record
  '[ "town"      >: Tx.Text
   , "apartment" >: Tx.Text ]

oneOf, noneOf :: String -> Parser Char
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

includeBanchi :: Parser String
includeBanchi = includeBanchi1
                <|> includeBanchi2
                <|> includeBanchi3

exceptNumbers :: Parser String
exceptNumbers = many1 $ noneOf numbers

parseAddress :: Parser String
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
