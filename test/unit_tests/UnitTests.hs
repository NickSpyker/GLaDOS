import LexerTest (testTokenize)
import BlockExprTest (testToBlock)
import ParserASTTest(testParse,
                    testParseNumberOperation, 
                    testParseBooleanOperation, 
                    testParseAST, 
                    testParseCmpOperation,
                    testParseInPrth,
                    testParseArray,
                    testParseBound,
                    testParseIncrAndBound,
                    testParseStruct,
                    testParseEnum,
                    testParseTypeDecl,
                    testParseIf,
                    testParseReturn,
                    testParseFunctionCall,
                    testParseFunction
                    )
import LibTest (testReadToString,
                testIsNumberOrDot,
                testIsNumber,
                testTrim,
                testFinishWith,
                testNbrToFormatString,
                testReadToString,
                testGetFilesContent,
                testHaveElemOf,
                testRmOcc,
                testExtractBtwQuot)
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
      testParse,
      testParseNumberOperation,
      testParseAST,
      testParseBooleanOperation,
      testParseCmpOperation,
      testParseInPrth,
      testParseArray,
      testParseBound,
      testParseIncrAndBound,
      testParseStruct,
      testParseEnum,
      testParseTypeDecl,
      testParseIf,
      testParseReturn,
      testParseFunctionCall,
      testReadToString,
      testIsNumberOrDot,
      testIsNumber,
      testTrim,
      testFinishWith,
      testNbrToFormatString,
      testReadToString   ,
      testGetFilesContent,
      testHaveElemOf,
      testRmOcc,
      testExtractBtwQuot,
      testParseFunction
    ]

main :: IO Counts
main = runTestTT tests
