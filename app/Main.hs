module Main (main, handleArgs, launchInterpreter, launchCompiler) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import System.Environment (getArgs)
import Usage (printHelp)
import Lexer (tokenize)


main :: IO ()
main = getArgs >>= handleArgs


handleArgs :: [String] -> IO ()
handleArgs [] = launchInterpreter []
handleArgs input
  | input `haveElemOf` ["-h", "--help"] = printHelp
  | input `haveElemOf` ["-i", "--interpret"] =
      getFilesContent (input `rmOcc` ["-i", "--interpret"]) >>= either putStrLn launchInterpreter
  | otherwise =
      getFilesContent input >>= either putStrLn launchCompiler


launchInterpreter :: [String] -> IO ()
launchInterpreter files = putStrLn "Interpret with files:" >> print files


launchCompiler :: [String] -> IO ()
launchCompiler (file : _) = print $ tokenize file
