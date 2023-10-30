module Main (main, handleArgs, launchInterpreter, launchCompiler) where


import Lib (getFilesContent, haveElemOf, rmOcc)
import BlockExpr (BExpr(..), tokensToBlock)
import System.Environment (getArgs)
import Prompt (launchPrompt)
import Usage (printHelp)
import Lexer (tokenize)


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


buildAstTree :: [String] -> [String] -> Either String BExpr
buildAstTree = buildAstTree' []
  where
    buildAstTree' :: [BExpr] -> [String] -> [String] -> Either String BExpr
    buildAstTree' acc _ [] = Right $ Program acc
    buildAstTree' acc (path : paths) (file : files) =
      case tokenize file of
        Left err     -> Left err
        Right tokens ->
          case tokensToBlock path tokens of
            Left err -> Left err
            Right mo -> buildAstTree' (acc ++ [mo]) paths files
    buildAstTree' _ _ _ = Left "unhandled error"


launchInterpreter :: [String] -> [String] -> IO ()
launchInterpreter paths files =
  case buildAstTree paths files of
    Left err -> putStrLn err
    Right expr -> launchPrompt expr


launchCompiler :: [String] -> [String] -> IO ()
launchCompiler paths files = print $ buildAstTree paths files
