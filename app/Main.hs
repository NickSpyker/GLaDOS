module Main (main) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import System.Directory (doesFileExist)
import Strain (getByteCodes, execute)
import System.Environment (getArgs)
import Prompt (launchPrompt)
import Usage (printHelp)
import Build (save, load)
import VM (run)


main :: IO ()
main = getArgs >>= handleArgs


handleArgs :: [String] -> IO ()
handleArgs [] = launchInterpreter [] []
handleArgs input
  | input `haveElemOf` ["build"] =
    getFilesContent (input `rmOcc` ["build"])
      >>= either putStrLn (launchBuild (input `rmOcc` ["build"]))
  | input `haveElemOf` ["run"] = launcheRun (input `rmOcc` ["run"]) 
  | input `haveElemOf` ["-h", "--help"] = printHelp
  | input `haveElemOf` ["-i", "--interpret"] =
      getFilesContent (input `rmOcc` ["-i", "--interpret"])
        >>= either putStrLn (launchInterpreter (input `rmOcc` ["-i", "--interpret"]))
  | otherwise =
      getFilesContent input
        >>= either putStrLn (launchRunFile input)


launchInterpreter :: [String] -> [String] -> IO ()
launchInterpreter paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> launchPrompt byc


launchRunFile :: [String] -> [String] -> IO ()
launchRunFile paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> execute byc


launchBuild :: [String] -> [String] -> IO ()
launchBuild paths files =
  case getByteCodes [] paths files of
    Left  err -> putStrLn err
    Right byc -> save "gl.run" byc


launcheRun :: [String] -> IO ()
launcheRun [file] = doesFileExist file >>= maybeLaunche file
  where
    maybeLaunche :: String -> Bool -> IO ()
    maybeLaunche f exist
      | exist     = load f >>= run
      | otherwise = putStrLn $ f ++ " does not exist (No such file or directory)"
launcheRun _ = return ()
