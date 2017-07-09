module WordFile
    ( dictionaryParser
      , wordLine
      , UnrankedPronunciation
    ) where

import Text.Parsec (ParseError, try)
import Text.Parsec.Char (noneOf, char, digit, satisfy, string, lower, upper)
import Text.Parsec.Combinator (many1, choice, chainl1, sepBy1)
import Control.Applicative ((<|>), many)
import Control.Monad
import Data.Char (isLetter, isDigit)
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import CommonParsers (whitespace, lexeme, Parser)
import qualified Data.Text as T

type UnrankedPronunciation = (String, [Phoneme])

dictionaryParser :: Parser [ UnrankedPronunciation ]
dictionaryParser = many1 wordLine

wordLine :: Parser UnrankedPronunciation
wordLine =
    try withComment <|> wordParser

withComment :: Parser UnrankedPronunciation
withComment = do
    w <- lexeme $ wordParser
    void $ lexeme $ char '#'
    void $ lexeme $ many $ noneOf "\n"
    return $ w

wordParser :: Parser UnrankedPronunciation
wordParser = do
    spelling <- spellingParser
    string " "
    pronounciation <- pronounciationParser
    return (spelling, pronounciation)

spellingParser =
    many1 $ noneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

pronounciationParser :: Parser [Phoneme]
pronounciationParser =
    many1 $ lexeme $ phonemeParser

phonemeParser :: Parser Phoneme
phonemeParser =
             try ( string "AA0" >> return ( AA Emp0 ))
         <|> try ( string "AA1" >> return ( AA Emp1 ))
         <|> try ( string "AA2" >> return ( AA Emp2 ))
         <|> try ( string "AA" >> return ( AA EmpNone ))
         <|> try ( string "AE0" >> return ( AE Emp0 ))
         <|> try ( string "AE1" >> return ( AE Emp1 ))
         <|> try ( string "AE2" >> return ( AE Emp2 ))
         <|> try ( string "AE" >> return ( AE EmpNone ))
         <|> try ( string "AH0" >> return ( AH Emp0 ))
         <|> try ( string "AH1" >> return ( AH Emp1 ))
         <|> try ( string "AH2" >> return ( AH Emp2 ))
         <|> try ( string "AH" >> return ( AH EmpNone ))
         <|> try ( string "AO0" >> return ( AO Emp0 ))
         <|> try ( string "AO1" >> return ( AO Emp1 ))
         <|> try ( string "AO2" >> return ( AO Emp2 ))
         <|> try ( string "AO" >> return ( AO EmpNone ))
         <|> try ( string "AW0" >> return ( AW Emp0 ))
         <|> try ( string "AW1" >> return ( AW Emp1 ))
         <|> try ( string "AW2" >> return ( AW Emp2 ))
         <|> try ( string "AW" >> return ( AW EmpNone ))
         <|> try ( string "AY0" >> return ( AY Emp0 ))
         <|> try ( string "AY1" >> return ( AY Emp1 ))
         <|> try ( string "AY2" >> return ( AY Emp2 ))
         <|> try ( string "AY" >> return ( AY EmpNone ))
         <|> try ( string "B" >> return B )
         <|> try ( string "CH" >> return CH )
         <|> try ( string "DH" >> return DH )
         <|> try ( string "D" >> return D )
         <|> try ( string "EH0" >> return ( EH Emp0 ))
         <|> try ( string "EH1" >> return ( EH Emp1 ))
         <|> try ( string "EH2" >> return ( EH Emp2 ))
         <|> try ( string "EH" >> return ( EH EmpNone ))
         <|> try ( string "ER0" >> return ( ER Emp0 ))
         <|> try ( string "ER1" >> return ( ER Emp1 ))
         <|> try ( string "ER2" >> return ( ER Emp2 ))
         <|> try ( string "ER" >> return ( ER EmpNone ))
         <|> try ( string "EY0" >> return ( EY Emp0 ))
         <|> try ( string "EY1" >> return ( EY Emp1 ))
         <|> try ( string "EY2" >> return ( EY Emp2 ))
         <|> try ( string "EY" >> return ( EY EmpNone ))
         <|> try ( string "F" >> return F )
         <|> try ( string "G" >> return G )
         <|> try ( string "HH" >> return HH )
         <|> try ( string "IH0" >> return ( IH Emp0 ))
         <|> try ( string "IH1" >> return ( IH Emp1 ))
         <|> try ( string "IH2" >> return ( IH Emp2 ))
         <|> try ( string "IH" >> return ( IH EmpNone ))
         <|> try ( string "IY0" >> return ( IY Emp0 ))
         <|> try ( string "IY1" >> return ( IY Emp1 ))
         <|> try ( string "IY2" >> return ( IY Emp2 ))
         <|> try ( string "IY" >> return ( IY EmpNone ))
         <|> try ( string "JH" >> return JH )
         <|> try ( string "K" >> return K )
         <|> try ( string "L" >> return L )
         <|> try ( string "M" >> return M )
         <|> try ( string "NG" >> return NG )
         <|> try ( string "N" >> return N )
         <|> try ( string "OW0" >> return ( OW Emp0 ))
         <|> try ( string "OW1" >> return ( OW Emp1 ))
         <|> try ( string "OW2" >> return ( OW Emp2 ))
         <|> try ( string "OW" >> return ( OW EmpNone ))
         <|> try ( string "OY0" >> return ( OY Emp0 ))
         <|> try ( string "OY1" >> return ( OY Emp1 ))
         <|> try ( string "OY2" >> return ( OY Emp2 ))
         <|> try ( string "OY" >> return ( OY EmpNone ))
         <|> try ( string "P" >> return P )
         <|> try ( string "R" >> return R )
         <|> try ( string "SH" >> return SH )
         <|> try ( string "S" >> return S )
         <|> try ( string "TH" >> return TH )
         <|> try ( string "T" >> return T )
         <|> try ( string "UH0" >> return ( UH Emp0 ))
         <|> try ( string "UH1" >> return ( UH Emp1 ))
         <|> try ( string "UH2" >> return ( UH Emp2 ))
         <|> try ( string "UH" >> return ( UH EmpNone ))
         <|> try ( string "UW0" >> return ( UW Emp0 ))
         <|> try ( string "UW1" >> return ( UW Emp1 ))
         <|> try ( string "UW2" >> return ( UW ( Emp2 ) ))
         <|> try ( string "UW" >> return ( UW EmpNone ))
         <|> try ( string "V" >> return V )
         <|> try ( string "W" >> return W )
         <|> try ( string "Y" >> return Y )
         <|> try ( string "ZH" >> return ZH )
         <|> try ( string "Z" >> return Z )


