import Test.HUnit
import WordFile (wordLine)
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import Text.Parsec (ParseError, eof, parse)
import Control.Applicative ((<*))
import CommonParsers (Parser)
import Data.Text as T hiding (tail, head)
import RankingFile
main :: IO ()
main = do
  runTestTT $ TestList [ testWordFileParser, testRankingParser]
  return ()

testWordFileParser :: Test
testWordFileParser = TestLabel "WordFile tests" $ TestList [
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

testRankingParser :: Test
testRankingParser = TestLabel "RankingFile tests" $ TestList [
      TestCase $
        assertEqual "one word's spelling"
        ( parseWithEof rankingLine "yow 42" ) $
          Right $ "yow"
      , TestCase $
        assertEqual "the lone line gets a rank of 1"
        ( head <$> parseWithEof rankingParser "yow 42" ) $
          Right $ ("yow", 1)
      , TestCase $
        assertEqual "the second line gets a rank of 2"
        ( snd . head . tail <$> (parseWithEof rankingParser "yow 42\nwow 42" ) ) $
          Right $ 2
    ]

makePronunciation :: String -> [Phoneme] -> (String, [Phoneme])
makePronunciation spelling phonemes =
  (spelling, phonemes)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
