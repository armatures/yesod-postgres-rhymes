{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module InsertWords (insertWords, rankPronunciation) where

import Import
import Rhymebook.Model
import Control.Monad.Logger
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import WordFile (UnrankedPronunciation)
import System.Environment
import Data.Text as T hiding (length)
import Data.Map as Map

insertWords :: [ UnrankedPronunciation ] -> Map String Int -> IO ()
insertWords dict rankMap = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll

    deleteWhere ([] :: [Filter Pronunciation])
    mapM (\w -> insert_ $ rankPronunciation w rankMap) dict
    putStrLn $ T.pack $ "inserted " ++ show ( length dict) ++ " words into the dictionary"
    return ()

rankPronunciation :: UnrankedPronunciation -> Map String Int -> Pronunciation
rankPronunciation w rankMap = Pronunciation (T.pack spelling) (phonemes) (ranking)
  where
    spelling = fst w
    phonemes = snd w
    ranking = Map.lookup spelling rankMap
