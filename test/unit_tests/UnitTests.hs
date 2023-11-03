import LexerTest (testTokenize)
import BlockExprTest (testToBlock)
import ParserASTTest(testParse)
import Test.HUnit

indicatorFunction :: Int -> Int
indicatorFunction 0 = 1
indicatorFunction _ = 0

tests :: Test
tests =
  TestList
    [
      testTokenize,
      testToBlock,
      testParse
    ]

main :: IO Counts
main = runTestTT tests
