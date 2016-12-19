{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.PostCSV where

import Import
import           GHC.List               ((!!), init)
import           Data.List              (intercalate)
import           Util.StrEnum           (split)
import           Util.Telephone         (telString, Telephone (..))
import           Meibo.Base             (meiboMain, Line (..))
import           Text.Read
import           Text.Printf            (printf)
import           Control.Arrow          ((&&&))
import           Control.Exception      (SomeException)
import qualified Data.Text              as Tx

type Bunkai     = String
type LineNumber = Int

data Person = P { name  :: Text
                , count :: Int
                , tels  :: [Text]
                , row   :: Int }

makePerson :: (Line, (Int, [Maybe Text])) -> Handler Person
makePerson (l, (c, tel)) = 
  return P { name = Tx.pack $ Meibo.Base.name l
           , count = c
           , tels = catMaybes tel
           , row = 0 }

personPrettyName :: Person -> Text
personPrettyName p 
  | Handler.PostCSV.count p == 1 = name'
  | otherwise = Tx.pack (printf "%s(×%d)" name' count')
  where name'  = Handler.PostCSV.name p
        count' = Handler.PostCSV.count p

parameterInfo :: String -> (Bunkai, [LineNumber])
parameterInfo str = (bunkai, map read (init rest))
  where bunkai:rest = split '&' str

getButtonValueList :: Int -> Handler [Maybe Text]
getButtonValueList row = mapM (runInputPost . iopt textField) rows
  where rows      = [ "button" <> pk row <> "-" <> pk n | n <- [0..5]]
        pk        = Tx.pack . show

getIntField :: Int -> Handler Int
getIntField row = runInputPost $ ireq intField ("count" <> Tx.pack (show row))
  
getCSVStatus :: Int -> Handler (Int, [Maybe Text])
getCSVStatus row = (,) <$> getIntField row
                       <*> getButtonValueList row

postPostCSVR :: String -> Handler Html
postPostCSVR parameters = do
  let (bunkai, indexes) = parameterInfo parameters
  datalist <- liftIO $ meiboMain bunkai
  
  let meibo = map (datalist!!) indexes

  info <- mapM getCSVStatus [0..(length indexes - 1)]

  hoge <- mapM makePerson $ zip meibo info
  defaultLayout $(widgetFile "postcsv")
