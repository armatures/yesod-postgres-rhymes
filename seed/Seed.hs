{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll

    deleteWhere ([] :: [Filter Phoneme])
    deleteWhere ([] :: [Filter PhonemeType])
    affricateId <- insert $ PhonemeType "affricate"
    aspirateId <- insert $ PhonemeType "aspirate"
    fricativeId <- insert $ PhonemeType "fricative"
    liquidId <- insert $ PhonemeType "liquid"
    nasalId <- insert $ PhonemeType "nasal"
    semivowelId <- insert $ PhonemeType "semivowel"
    stopId <- insert $ PhonemeType "stop"
    vowelId <- insert $ PhonemeType "vowel"
    insert_ $ Phoneme "AA" vowelId
    insert_ $ Phoneme "AE" vowelId
    insert_ $ Phoneme "AH" vowelId
    insert_ $ Phoneme "AO" vowelId
    insert_ $ Phoneme "AW" vowelId
    insert_ $ Phoneme "AY" vowelId
    insert_ $ Phoneme "B" stopId
    insert_ $ Phoneme "CH" affricateId
    insert_ $ Phoneme "D" stopId
    insert_ $ Phoneme "DH" fricativeId
    insert_ $ Phoneme "EH" vowelId
    insert_ $ Phoneme "ER" vowelId
    insert_ $ Phoneme "EY" vowelId
    insert_ $ Phoneme "F" fricativeId
    insert_ $ Phoneme "G" stopId
    insert_ $ Phoneme "HH" aspirateId
    insert_ $ Phoneme "IH" vowelId
    insert_ $ Phoneme "IY" vowelId
    insert_ $ Phoneme "JH" affricateId
    insert_ $ Phoneme "K" stopId
    insert_ $ Phoneme "L" liquidId
    insert_ $ Phoneme "M" nasalId
    insert_ $ Phoneme "N" nasalId
    insert_ $ Phoneme "NG" nasalId
    insert_ $ Phoneme "OW" vowelId
    insert_ $ Phoneme "OY" vowelId
    insert_ $ Phoneme "P" stopId
    insert_ $ Phoneme "R" liquidId
    insert_ $ Phoneme "S" fricativeId
    insert_ $ Phoneme "SH" fricativeId
    insert_ $ Phoneme "T" stopId
    insert_ $ Phoneme "TH" fricativeId
    insert_ $ Phoneme "UH" vowelId
    insert_ $ Phoneme "UW" vowelId
    insert_ $ Phoneme "V" fricativeId
    insert_ $ Phoneme "W" semivowelId
    insert_ $ Phoneme "Y" semivowelId
    insert_ $ Phoneme "Z" fricativeId
    insert_ $ Phoneme "ZH" fricativeId
