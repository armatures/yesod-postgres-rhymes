{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lib ( seed
  , rankPronunciation)
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
import Data.Map as Map

seed :: IO ()
seed = do
  eRankings <- parseRankings
  eDictionary <- parseDict

  either
    ( printParseFailure rankingPath )
    ( \rankingsMap ->
    either
      ( printParseFailure dictionaryPath )
      ( \dictionary -> insertWords dictionary rankingsMap )
      eDictionary
    ) eRankings

dictionaryPath = "/Users/charliebevis/workspace/cmudict/cmudict.dict"
rankingPath = "/Users/charliebevis/workspace/cmudict/word_frequency.txt"

printParseFailure :: String -> ParseError -> IO ()
printParseFailure dictionaryPath _ =
         putStrLn $ T.pack $
           "failed to parse the dictionary at " ++ dictionaryPath

parseDict :: IO (Either ParseError [UnrankedPronunciation])
parseDict = parseFromFile dictionaryParser dictionaryPath

parseRankings :: IO (Either ParseError (Map String Int))
parseRankings = parseFromFile rankingParser rankingPath

insertWords :: [ UnrankedPronunciation ] -> Map String Int -> IO ()
insertWords dict rankMap = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll

    deleteWhere ([] :: [Filter Pronunciation])
    mapM (\w -> insert_ $ rankPronunciation w rankMap) dict
    putStrLn $ T.pack $ "inserted " ++ show ( Import.length dict) ++ " words into the dictionary"
    return ()

rankPronunciation :: UnrankedPronunciation -> Map String Int -> Pronunciation
rankPronunciation w rankMap = Pronunciation (T.pack spelling) (phonemes) (ranking)
  where
    spelling = fst w
    phonemes = snd w
    ranking = Map.lookup spelling rankMap
