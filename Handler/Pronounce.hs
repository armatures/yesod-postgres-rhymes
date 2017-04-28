module Handler.Pronounce where

import Import
getPronounceR :: Text -> Handler Html
getPronounceR spelling =
    defaultLayout $ do
        setTitle $ toHtml spelling
        $(widgetFile "pronounce")

