module Handler.QForm where

import Import

data QForm = Q { qBunkai :: [Text]
               , qName   :: [Text] }

postQFormR :: Handler Html
postQFormR = error "Not yet implemented: postQFormR"
