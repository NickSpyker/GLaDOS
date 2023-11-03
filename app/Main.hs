module Main (main) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import System.Environment (getArgs)
import Strain (getByteCodes)
import Prompt (launchPrompt)
import Usage (printHelp)


main :: IO ()
main = getArgs >>= handleArgs


handleArgs :: [String] -> IO ()
handleArgs [] = launchInterpreter [] []
handleArgs input
  | input `haveElemOf` ["-h", "--help"] = printHelp
  | input `haveElemOf` ["-i", "--interpret"] =
      getFilesContent (input `rmOcc` ["-i", "--interpret"])
        >>= either putStrLn (launchInterpreter (input `rmOcc` ["-i", "--interpret"]))
  | otherwise =
      getFilesContent input >>= either putStrLn (launchCompiler input)


launchInterpreter :: [String] -> [String] -> IO ()
launchInterpreter paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> launchPrompt byc


launchCompiler :: [String] -> [String] -> IO ()
launchCompiler paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> print byc
