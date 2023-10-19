module Main (main) where


import Lib (getFilesContents, haveElemOf, rmOcc)
import System.Environment (getArgs)
import Usage (printHelp)


main :: IO ()
main = getArgs >>= handleArgs


handleArgs :: [String] -> IO ()
handleArgs [] = launchInterpreter []
handleArgs input
  | input `haveElemOf` ["-h", "--help"] = printHelp
  | input `haveElemOf` ["-i", "--interpret"] =
      getFilesContents (input `rmOcc` ["-i", "--interpret"]) >>= either putStrLn launchInterpreter
  | otherwise =
      getFilesContents input >>= either putStrLn launchCompiler


launchInterpreter :: [String] -> IO ()
launchInterpreter files = putStrLn "Interpret with files:" >> print files


launchCompiler :: [String] -> IO ()
launchCompiler files = putStrLn "Compiler with files:" >> print files
