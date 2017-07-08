import Test.HUnit
import WordFile (wordLine)
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import Text.Parsec (ParseError, eof, parse)
import Control.Applicative ((<*))
import CommonParsers (Parser)
import Data.Text as T

main :: IO ()
main = do
  runTestTT $ TestLabel "WordFile tests" $ TestList [
      TestCase $
        assertEqual "basic spelling and pronunciation"
        ( parseWithEof wordLine "yow Y OW" ) $
          Right $ makePronunciation "yow" [Y, OW EmpNone]
      , TestCase $
        assertEqual "blank comment"
        ( parseWithEof wordLine "yow Y OW1 #" ) $
          Right $ makePronunciation "yow" [Y, OW Emp1]
      , TestCase $
        assertEqual "dash in spelling, comment"
        ( parseWithEof wordLine "yew-chow Y UW2 CH OW1 # dash in spelling") $
          Right $ makePronunciation "yew-chow" [Y, UW Emp2, CH, OW Emp1]
      , TestCase $
        assertEqual "weird comment"
        ( parseWithEof wordLine "yow Y OW # comment'with_weird\" characters" ) $
          Right $ makePronunciation "yow" [Y, OW EmpNone]
    ]
  return ()

makePronunciation :: String -> [Phoneme] -> Pronunciation
makePronunciation spelling phonemes =
  Pronunciation (T.pack spelling) phonemes Nothing

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
