{-# LANGUAGE OverloadedStrings #-}
module Handler.Search where

import Import                     hiding (many)
import Text.Parsec
import Handler.InterSection       (getAll)

parseField :: Text -> [String]
parseField = either mempty id . parse parser mempty
  where parser = sepBy (many (noneOf " ")) (many1 (char ' '))

fromField :: [String] -> [Person] -> [Person]
fromField query = undefined

getSearchR :: Handler Html
getSearchR = error "Not yet implemented"

postSearchR :: Handler Html
postSearchR = do
  field <- runInputPost $ ireq textField "query"
  meibo <- getAll
  let result = parseField field
  defaultLayout $ do
    $(widgetFile "Search")
