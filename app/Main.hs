module Main (main) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import Strain (getByteCodes, execute)
import System.Environment (getArgs)
import Prompt (launchPrompt)
import Usage (printHelp)


main :: IO ()
main = getArgs >>= handleArgs


handleArgs :: [String] -> IO ()
handleArgs [] = launchInterpreter [] []
handleArgs input
  | input `haveElemOf` ["build"] = putStrLn "TODO: build"
  | input `haveElemOf` ["run"]   = putStrLn "TODO: run"
  | input `haveElemOf` ["-h", "--help"] = printHelp
  | input `haveElemOf` ["-i", "--interpret"] =
      getFilesContent (input `rmOcc` ["-i", "--interpret"])
        >>= either putStrLn (launchInterpreter (input `rmOcc` ["-i", "--interpret"]))
  | otherwise =
      getFilesContent input >>= either putStrLn (launchRunFile input)


launchInterpreter :: [String] -> [String] -> IO ()
launchInterpreter paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> launchPrompt byc


launchRunFile :: [String] -> [String] -> IO ()
launchRunFile = execute []
