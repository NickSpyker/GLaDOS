module LibTest (testReadToString,
                testIsNumberOrDot,
                testIsNumber,
                testTrim,
                testFinishWith,
                testNbrToFormatString,
                testReadToString,
                testGetFilesContent,
                testHaveElemOf,
                testRmOcc,
                testExtractBtwQuot
                ) where
import Lib (readToString, getFilesContent, haveElemOf, rmOcc, isNumber, isNumberOrDot, extractBtwQuot, trim, finishWith, nbrToFormatString)
import Test.HUnit ( (~:), (~?=), Test(TestList) )

import Test.HUnit
import System.IO (withFile, IOMode(..), hPutStr, hClose)
import System.Directory (removeFile, doesFileExist)
import Control.Exception (evaluate, try, IOException)

setupTestFile :: FilePath -> String -> IO ()
setupTestFile path content = withFile path WriteMode (\h -> hPutStr h content)

cleanupTestFile :: FilePath -> IO ()
cleanupTestFile path = removeFile path

testGetFilesContent :: Test
testGetFilesContent = TestList
    [ "test reading multiple existing files"
        ~: do
            let testFilePath1 = "test_file1.txt"
            let testFilePath2 = "test_file2.txt"
            let fileContent1 = "Content for file1"
            let fileContent2 = "Content for file2"
            setupTestFile testFilePath1 fileContent1
            setupTestFile testFilePath2 fileContent2
            readResults <- getFilesContent [testFilePath1, testFilePath2]
            cleanupTestFile testFilePath1
            cleanupTestFile testFilePath2
            assertEqual "Should read contents of both files" (Right [fileContent1, fileContent2]) readResults
    ]

-- Tests for haveElemOf
testHaveElemOf :: Test
testHaveElemOf = TestList [
  "Both lists empty" ~: haveElemOf [] [] ~?= False,
  "First list empty" ~: haveElemOf [] ["a"] ~?= False,
  "Second list empty" ~: haveElemOf ["a"] [] ~?= False,
  "No common elements" ~: haveElemOf ["a"] ["b", "c"] ~?= False,
  "Common elements exist" ~: haveElemOf ["a", "b"] ["b", "c"] ~?= True
    ]

-- Tests for rmOcc
testRmOcc :: Test
testRmOcc = TestList [
  "Removing from empty list" ~: rmOcc [] ["a"] ~?= [],
  "Removing non-existent elements" ~: rmOcc ["a", "b", "c"] ["d", "e"] ~?= ["a", "b", "c"],
  "Removing existing elements" ~: rmOcc ["a", "b", "d", "b"] ["b"] ~?= ["a", "d"]
    ]

testReadToString :: Test
testReadToString = TestList
    [ "test read existing file"
        ~: do
            let testFilePath = "test_file.txt"
            let fileContent = "This is a test file."
            setupTestFile testFilePath fileContent
            readResult <- readToString testFilePath
            cleanupTestFile testFilePath
            assertEqual "Should read the content successfully" (Right fileContent) readResult

    , "test read non-existing file"
        ~: do
            let testFilePath = "non_existing_file.txt"
            readResult <- readToString testFilePath
            assertEqual "Should return error for non-existing file" True (either (const True) (const False) readResult)
    ]

testIsNumber :: Test
testIsNumber = TestList [
  "Empty string is not a number" ~: isNumber "" ~?= False,
  "Single digit is a number" ~: isNumber "7" ~?= True,
  "String with only digits is a number" ~: isNumber "123456" ~?= True,
  "String with letters is not a number" ~: isNumber "123a456" ~?= False
  ]

-- Test for isNumberOrDot function
testIsNumberOrDot :: Test
testIsNumberOrDot = TestList [
  "Empty string is not a number or dot" ~: isNumberOrDot "" ~?= False,
  "Single dot is not a valid number or dot sequence" ~: isNumberOrDot "." ~?= True,
  "Dot with numbers is a valid sequence" ~: isNumberOrDot "12.34" ~?= True,
  "Multiple dots are not valid" ~: isNumberOrDot "12.34.56" ~?= False
  ]

-- Test for trim function
testTrim :: Test
testTrim = TestList [
  "Trimming a string" ~: trim " hello " ~?= "hello",
  "Trimming a string with newline and tabs" ~: trim "\n\t hello \n\t" ~?= "hello",
  "String without leading or trailing whitespace remains unchanged" ~: trim "hello" ~?= "hello",
  "Trimming a string with \r" ~: trim "\n\r hello \n\t" ~?= "hello"

  ]

testExtractBtwQuot :: Test
testExtractBtwQuot = TestList [
  "Extract between quotes with no escape characters" ~: extractBtwQuot "\"Hello, world!\"" ~?= Just "Hello, world!",
  "Extract between quotes with escape character" ~: extractBtwQuot "\"Hello, \\\"world!\\\"\"" ~?= Just "Hello, \\\"world!\\\"",
  "Return Nothing for string without closing quote" ~: extractBtwQuot "\"Hello, world!" ~?= Nothing,
  "Return Nothing for string without quotes" ~: extractBtwQuot "Hello, world!" ~?= Nothing,
  "Return Nothing for empty string" ~: extractBtwQuot "" ~?= Nothing,
  "Handle strings with only one quote" ~: extractBtwQuot "\"" ~?= Nothing,
  "Handle strings with escaped backslash" ~: extractBtwQuot "\"escaped \\\\ backslash\"" ~?= Just "escaped \\\\ backslash"
  ]

-- Test for finishWith function
testFinishWith :: Test
testFinishWith = TestList [
  "String finishes with pattern" ~: finishWith "hello" "lo" ~?= True,
  "String does not finish with pattern" ~: finishWith "hello" "he" ~?= False,
  "Empty string and empty pattern" ~: finishWith "" "" ~?= True
  ]

-- Test for nbrToFormatString function
testNbrToFormatString :: Test
testNbrToFormatString = TestList [
  "Formatting single digit number" ~: nbrToFormatString 5 ~?= "005",
  "Formatting two-digit number" ~: nbrToFormatString 50 ~?= "050",
  "Formatting three-digit number" ~: nbrToFormatString 500 ~?= "500"
  ]
