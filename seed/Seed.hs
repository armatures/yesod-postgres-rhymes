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
    aa <- insert $ Phoneme "AA" vowelId
    ae <- insert $ Phoneme "AE" vowelId
    ah <- insert $ Phoneme "AH" vowelId
    ao <- insert $ Phoneme "AO" vowelId
    aw <- insert $ Phoneme "AW" vowelId
    ay <- insert $ Phoneme "AY" vowelId
    b <- insert $ Phoneme "B" stopId
    ch <- insert $ Phoneme "CH" affricateId
    d <- insert $ Phoneme "D" stopId
    dh <- insert $ Phoneme "DH" fricativeId
    eh <- insert $ Phoneme "EH" vowelId
    er <- insert $ Phoneme "ER" vowelId
    ey <- insert $ Phoneme "EY" vowelId
    f <- insert $ Phoneme "F" fricativeId
    g <- insert $ Phoneme "G" stopId
    hh <- insert $ Phoneme "HH" aspirateId
    ih <- insert $ Phoneme "IH" vowelId
    iy <- insert $ Phoneme "IY" vowelId
    jh <- insert $ Phoneme "JH" affricateId
    k <- insert $ Phoneme "K" stopId
    l <- insert $ Phoneme "L" liquidId
    m <- insert $ Phoneme "M" nasalId
    n <- insert $ Phoneme "N" nasalId
    ng <- insert $ Phoneme "NG" nasalId
    ow <- insert $ Phoneme "OW" vowelId
    oy <- insert $ Phoneme "OY" vowelId
    p <- insert $ Phoneme "P" stopId
    r <- insert $ Phoneme "R" liquidId
    s <- insert $ Phoneme "S" fricativeId
    sh <- insert $ Phoneme "SH" fricativeId
    t <- insert $ Phoneme "T" stopId
    th <- insert $ Phoneme "TH" fricativeId
    uh <- insert $ Phoneme "UH" vowelId
    uw <- insert $ Phoneme "UW" vowelId
    v <- insert $ Phoneme "V" fricativeId
    w <- insert $ Phoneme "W" semivowelId
    y <- insert $ Phoneme "Y" semivowelId
    z <- insert $ Phoneme "Z" fricativeId
    insert_ $ Phoneme "ZH" fricativeId
    -- insert_ $ Pronounciation "BOW" [b, ow]
