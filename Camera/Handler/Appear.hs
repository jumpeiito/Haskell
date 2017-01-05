{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}
module Handler.Appear where

import Import
import Data.List ((!!))
import Data.Array
import Handler.InterSection
import Text.Printf
import qualified Data.Text              as Tx
-- import           Meibo.Base (meiboMain, telephoneStr, addressStr, Line (..))

data AppStatus = Stat { pid   :: Int
                      , meibo :: Person
                      , count :: Integer
                      , tels  :: [Maybe Text] } deriving Show

personPrettyName :: AppStatus -> Text
personPrettyName p 
  | count' == 1 = Tx.pack name'
  | otherwise   = Tx.pack (printf "%s(×%d)" name' count')
  where name'   = personName $ meibo p
        count'  = Handler.Appear.count p

getButtonValueList :: Int -> Handler [Maybe Text]
getButtonValueList row = mapM (runInputPost . iopt textField) rows
  where rows      = [ "button" <> pk row <> "-" <> pk n | n <- [0..5]::[Int]]
        pk        = Tx.pack . show
        
getIntField :: Int -> Handler (Maybe Integer)
getIntField row = runInputPost $ iopt intField ("count" <> Tx.pack (show row))
  
getCSVStatus :: Int -> Handler (Integer, [Maybe Text])
getCSVStatus row = do
  values <- getButtonValueList row
  number <- getIntField row
  case (number, any isJust values) of
    (Just num, True)  -> return (num, values)
    _ -> return (0, mempty)

getCSVStatusAll :: Array Int Person -> Handler [AppStatus]
getCSVStatusAll meiboRaw = 
  reverse <$> foldM func [] [0..1000]
  where func seed row = do
          status <- getCSVStatus row
          case status of
            (0, _) -> return seed
            (r, v) -> do
              let mb = meiboRaw ! row
              return $ (Stat row mb r v) : seed

postAppearR :: Handler Html
postAppearR = do
  meiboRaw <- listArray (0, 1000) <$> getAll
  dataMain <- getCSVStatusAll meiboRaw

  defaultLayout $ do
    $(widgetFile "Appear")
