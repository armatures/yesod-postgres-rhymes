{-# LANGUAGE FlexibleInstances #-}

module Rhymebook.Model where --(Emphasis, Phoneme, Pronunciation)  where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Text as T

data Emphasis = Emp2 | Emp1 | Emp0 | EmpNone
    deriving (Eq, Read, Show)
derivePersistField "Emphasis"

instance ToJSON Emphasis where
    toJSON a = String $ T.pack (show a)

instance FromJSON Emphasis where
    parseJSON = undefined

data Phoneme = AA Emphasis | AE Emphasis | AH Emphasis | AO Emphasis |
    AW Emphasis | AY Emphasis | B | CH | D | DH | EH Emphasis | ER Emphasis |
    EY Emphasis | F | G | HH | IH Emphasis | IY Emphasis | JH | K | L | M |
    N | NG | OW Emphasis | OY Emphasis | P | R | S | SH | T | TH |
    UH Emphasis | UW Emphasis | V | W | Y | Z | ZH
    deriving (Show, Read, Eq)
derivePersistField "Phoneme"

instance ToJSON Phoneme where
    toJSON a = String $ T.pack (show a)

instance FromJSON Phoneme where
    parseJSON = undefined

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

