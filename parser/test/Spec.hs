import Test.HUnit
import Rhymebook.Model (Phoneme(..), Emphasis(..), Pronunciation(..))
import Text.Parsec (ParseError, eof, parse)
import Control.Applicative ((<*))
import Data.Text as T hiding (tail, head)
import Seed
import Parsers.CommonParsers (Parser)
import Parsers.WordFile (wordLine)
import Parsers.RankingFile
import InsertWords (rankPronunciation)
import Data.Map as Map

main :: IO ()
main = do
  runTestTT $ TestList [ testWordFileParser
    , testRankingParser
    , testSeed
    ]
  return ()

testWordFileParser :: Test
testWordFileParser = TestLabel "WordFile tests" $ TestList [
      TestCase $
        assertEqual "basic spelling and pronunciation"
        (Right $ makePronunciation "yow" [Y, OW EmpNone]) $
          parseWithEof wordLine "yow Y OW"
      , TestCase $
        assertEqual "spellings with special character at start"
        ( Right $ makePronunciation "'bout" [B, AW Emp1, T] ) $
          parseWithEof wordLine "'bout B AW1 T"
      , TestCase $
        assertEqual "blank comment"
        ( Right $ makePronunciation "yow" [Y, OW Emp1] ) $
          parseWithEof wordLine "yow Y OW1 #"
      , TestCase $
        assertEqual "spelling with parenthesizedDigit"
        ( Right $ makePronunciation "a(2)" [EY Emp1] ) $
          parseWithEof wordLine "a(2) EY1"
      , TestCase $
        assertEqual "dash in spelling, comment"
        ( Right $ makePronunciation "yew-chow" [Y, UW Emp2, CH, OW Emp1] ) $
          parseWithEof wordLine "yew-chow Y UW2 CH OW1 # dash in spellin"
      , TestCase $
        assertEqual "weird comment"
        ( Right $ makePronunciation "yow" [Y, OW EmpNone] ) $
          parseWithEof wordLine "yow Y OW # comment'with_eird\" characters"
    ]

testRankingParser :: Test
testRankingParser = TestLabel "RankingFile tests" $ TestList [
      TestCase $
        assertEqual "one word's spelling"
        ( Right $ "yow" ) $
          parseWithEof rankingLine "yow 42"
      , TestCase $
        assertEqual "the lone line gets a rank of 1"
        (Right 1) $
          ( flip (!) "yow" ) <$> parseWithEof rankingParser "yow 42"
      , TestCase $
        assertEqual "the second line gets a rank of 2"
        (Right 2) $
           ( flip (!) "second" ) <$> (parseWithEof rankingParser "yow 42\nsecond 42" )
    ]

testSeed :: Test
testSeed = TestLabel "ranking lookup tests" $ TestList [
      TestCase $
        assertEqual "empty map gives ranking of Nothing"
          ( Pronunciation ( T.pack "y" ) [Y] Nothing ) $
             rankPronunciation ("y", [Y]) ( Map.fromList [] )
      , TestCase $
        assertEqual "map with key present gives real ranking"
         ( Pronunciation ( T.pack "xY" ) [Y] ( Just 21 ) ) $
           ( rankPronunciation ("xY", [Y]) ( Map.fromList [("xY",21)] ) )
      , TestCase $
        assertEqual "rank ignores leading apostrophe"
         ( Pronunciation ( T.pack "'Y" ) [Y] ( Just 21 ) ) $
           ( rankPronunciation ("'Y", [Y]) ( Map.fromList [("Y",21)] ) )
      , TestCase $
        assertEqual "rank lookup ignores spelling's (2) suffix"
         ( Pronunciation ( T.pack "y(2)" ) [Y] ( Just 21 ) ) $
           ( rankPronunciation ("y(2)", [Y]) ( Map.fromList [("y",21)] ) )
      , TestCase $
        assertEqual "rank lookup ignores spelling's 's suffix"
         ( Pronunciation ( T.pack "robot's(2)" ) [R] ( Just 21 ) ) $
           ( rankPronunciation ("robot's(2)", [R]) ( Map.fromList [("robot",21)] ) )
         ]

makePronunciation :: String -> [Phoneme] -> (String, [Phoneme])
makePronunciation spelling phonemes =
  (spelling, phonemes)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
