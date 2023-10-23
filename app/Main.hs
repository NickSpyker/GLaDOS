module Main (main, handleArgs, launchInterpreter, launchCompiler) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import BlockExpr (BExpr(..), tokensToBlock)
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


buildAstTree :: [String] -> Either String BExpr
buildAstTree = buildAstTree' []
  where
    buildAstTree' :: [BExpr] -> [String] -> Either String BExpr
    buildAstTree' acc [] = Right $ Program acc
    buildAstTree' acc (file : files) =
      case tokenize file of
        Left err     -> Left err
        Right tokens ->
          case tokensToBlock file tokens of
            Left err -> Left err
            Right bl -> buildAstTree' (acc ++ [bl]) files


launchInterpreter :: [String] -> IO ()
launchInterpreter files = print $ buildAstTree files


launchCompiler :: [String] -> IO ()
launchCompiler files = print $ buildAstTree files
