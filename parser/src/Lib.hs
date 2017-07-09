{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lib (seed)
  where
import Import
import Rhymebook.Model
import Control.Monad.Logger
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import System.Environment
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)
import Control.Monad
import WordFile
import RankingFile (rankingParser)
import Control.Monad.IO.Class (liftIO)
import Data.Text as T

seed :: IO ()
seed = do
  rankings <- parseRankings
  dictionary <- parseDict
  either
         (printParseFailure dictionaryPath)
         ( \proList -> insertWords proList )
         dictionary

dictionaryPath = "/Users/charliebevis/workspace/cmudict/cmudict.dict"
rankingPath = "/Users/charliebevis/workspace/cmudict/word_frequency.txt"

printParseFailure :: String -> ParseError -> IO ()
printParseFailure dictionaryPath _ =
         putStrLn $ T.pack $
           "failed to parse the dictionary at " ++ dictionaryPath

parseDict :: IO (Either ParseError [UnrankedPronunciation])
parseDict = parseFromFile dictionaryParser dictionaryPath

parseRankings :: IO (Either ParseError (Map String Integer))
parseRankings = parseFromFile rankingParser rankingPath

insertWords :: [ UnrankedPronunciation ] -> IO ()
insertWords dict = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll

    deleteWhere ([] :: [Filter Pronunciation])
    mapM (\w -> insert_ $ Pronunciation (T.pack $ fst w) (snd w) Nothing) dict
    putStrLn $ T.pack $ "inserted " ++ show ( Import.length dict) ++ " words into the dictionary"
    return ()
