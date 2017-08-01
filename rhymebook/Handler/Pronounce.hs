module Handler.Pronounce where

import Import
getPronounceR :: Text -> Handler Value
getPronounceR _spelling = do
    pros <- runDB $ selectList [PronunciationSpelling ==. _spelling] [LimitTo 1]

    return $ object ["pronunciations" .= pros]

