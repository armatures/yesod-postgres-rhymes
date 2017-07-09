module RankingFile
           where

import Text.Parsec (ParseError, try)
import Text.Parsec.Char (noneOf, char, digit, satisfy, string, lower, upper)
import Text.Parsec.Combinator (many1, choice, chainl1, sepBy1)
import Control.Applicative ((<|>), many)
import Control.Monad
import Data.Char (isLetter, isDigit)
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import CommonParsers (whitespace, lexeme, Parser)
import Data.Map as Map

rankingParser :: Parser (Map String Integer)
rankingParser = Map.fromList <$> associationList
                 where associationList = (flip zip [1..]) <$> (many1 rankingLine)

rankingLine :: Parser String
rankingLine = do
           spelling <- lexeme $ many1 $ noneOf " \t"
           void $ lexeme $ many1 digit
           return spelling

