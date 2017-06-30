{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import WordFile

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  let dictionary = parseDict

  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $
    runMigration migrateAll >>
      deleteWhere ([] :: [Filter Pronunciation]) >>
        -- map (\w -> insert_ ( Pronunciation "w" [W] Nothing ) ) dictionary
        insertPronunciation <$> dictionary -- >>= \ids ->
          -- useDictionary dictionary
          -- ( insert_ $ Pronunciation "wow" [W, OW EmpNone] Nothing )
          -- liftIO $ print ids

insertPronunciation =
  \dict -> case dict of
       Left e ->
         fail
       Right proList ->
         fail
         -- insert ( Pronunciation "w" [W] Nothing ) )

parseDict :: IO (Either ParseError [Pronunciation])
parseDict = parseFromFile dictionaryParser "~/workspace/cmudict/cmudict.dict"

-- useDictionary :: Either ParseError [Pronunciation] -> ReaderT SqlBackend (Control.Monad.Logger.LoggingT IO) --IO ()
useDictionary d =
  case d of
    Left _ -> --print "wah"
      fail
    Right xs -> --print xs
      fail
