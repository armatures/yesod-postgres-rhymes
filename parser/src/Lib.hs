{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lib ( seed )
    where
import Import
import Rhymebook.Model
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)
import Control.Monad
import Data.Text as T
import Data.Map as Map
import WordFile
import RankingFile (rankingParser)
import InsertWords (insertWords)

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

