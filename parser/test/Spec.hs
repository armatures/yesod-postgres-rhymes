import Test.HUnit
import WordFile (wordLine)
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import Text.Parsec (ParseError, eof, parse)
import Control.Applicative ((<*))
import CommonParsers (Parser)
import Data.Text as T hiding (tail, head)
import Seed
import RankingFile
import InsertWords
import Data.Map as Map

main :: IO ()
main = do
  runTestTT $ TestList [ testWordFileParser, testRankingParser, testSeed]
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
        ( ( flip (!) "yow" ) <$> parseWithEof rankingParser "yow 42" ) $
          Right $ 1
      , TestCase $
        assertEqual "the second line gets a rank of 2"
        ( ( flip (!) "second" ) <$> (parseWithEof rankingParser "yow 42\nsecond 42" ) ) $
          Right $ 2
    ]

testSeed :: Test
testSeed = TestLabel "Seed tests" $ TestList [
      TestCase $
        assertEqual "empty map gives ranking of Nothing"
          ( rankPronunciation ("spellingY", [Y]) ( Map.fromList [] ) ) $
            ( Pronunciation ( T.pack "spellingY" ) [Y] Nothing )
      , TestCase $
        assertEqual "map with key present gives real ranking"
          ( rankPronunciation ("spellingY", [Y]) ( Map.fromList [("spellingY",21)] ) ) $
            ( Pronunciation ( T.pack "spellingY" ) [Y] ( Just 21 ) )
                                           ]

makePronunciation :: String -> [Phoneme] -> (String, [Phoneme])
makePronunciation spelling phonemes =
  (spelling, phonemes)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
