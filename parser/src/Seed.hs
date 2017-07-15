{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Seed ( seed )
    where
import Import
import Rhymebook.Model
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)
import Control.Monad
import Data.Text as T
import Data.Map as Map
import Parsers.WordFile
import Parsers.RankingFile (rankingParser)
import InsertWords (insertWords)

seed :: IO ()
seed = do
  eRankings <- parseFromFile rankingParser rankingPath
  eDictionary <- parseFromFile dictionaryParser dictionaryPath

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
printParseFailure dictionaryPath e =
         putStrLn $ T.pack $
           show e

