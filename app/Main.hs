module Main (main, handleArgs, launchInterpreter, launchCompiler) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import System.Environment (getArgs)
import Prompt (launchPrompt)
import ParserAST (Ast(..))
import Usage (printHelp)
import Strain (getAst)


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


buildAstTree :: [String] -> [String] -> Either String Ast
buildAstTree = buildAstTree' []
  where
    buildAstTree' :: [Ast] -> [String] -> [String] -> Either String Ast
    buildAstTree' acc _ [] = Right $ Program acc
    buildAstTree' acc (path : paths) (file : files) =
      case getAst path file of
        Left  err -> Left err
        Right ast -> buildAstTree' (acc ++ [ast]) paths files
    buildAstTree' _ _ _ = Left "unhandled error"


launchInterpreter :: [String] -> [String] -> IO ()
launchInterpreter paths files =
  case buildAstTree paths files of
    Left  err -> putStrLn err
    Right ast -> launchPrompt ast


launchCompiler :: [String] -> [String] -> IO ()
launchCompiler paths files =
  case buildAstTree paths files of
    Left  err -> putStrLn err
    Right ast -> print ast
