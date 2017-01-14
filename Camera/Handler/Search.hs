{-# LANGUAGE OverloadedStrings #-}
module Handler.Search where

import Import                     hiding (many)
import Text.Parsec
import Handler.InterSection       (getAll)

-- 1つ以上の半角スペースで区切られた入力文字をリストにして返す。
parseField :: Text -> [String]
parseField = either mempty id . parse parser mempty
  where parser = many (noneOf " ") `sepBy` many1 (char ' ')

fromFieldPersonal :: [String] -> Person -> Bool
fromFieldPersonal queries p = or $ map includeP queries
  -- map ($ p) [f1, f2] --> [f1 p, f2 p]
  -- map ((query `isInfixOf`) . ($ p)) [f1, f2] --> [ query `isInfixOf` f1 p, query `isInfixOf` f2 p]
  where includeP query = or $ map ((query `isInfixOf`) . ($ p))
                            [ personName, personTel, personAddress ]

fromField :: [String] -> [Person] -> [Person]
fromField queries = filter (fromFieldPersonal queries)

getSearchR :: Handler Html
getSearchR = error "Not yet implemented"

postSearchR :: Handler Html
postSearchR = do
  field <- runInputPost $ ireq textField "query"
  let query = parseField field
  meibo <- fromField query <$> getAll
  defaultLayout $ do
    $(widgetFile "Search")
