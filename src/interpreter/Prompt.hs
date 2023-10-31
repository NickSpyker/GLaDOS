module Prompt (launchPrompt) where


import System.Console.Haskeline (runInputT, defaultSettings, historyFile, autoAddHistory, InputT, getInputLine, outputStrLn, outputStr)
import DebugOutput (printDebugTokens, printDebugBlockExpression, printDebugAst)
import Strain (getTokens, getBlockExpr, getAst)
import Data.List (isPrefixOf, isSubsequenceOf)
import PromptUsage (printPromptHelp)
import Data.Version (showVersion)
import Paths_glados (version)
import System.Info (os)
import ParserAST (Ast)
import Lib (trim)


launchPrompt :: Ast -> IO ()
launchPrompt _ = printHeader >> runInputT
  defaultSettings { historyFile = Just ".glados_history", autoAddHistory = True }
  promptLoop


printHeader :: IO ()
printHeader = putStrLn $
  "GLaDOS Interpreter\nVersion "
    ++ showVersion version
    ++ " on "
    ++ os
    ++ "\n\nType !help for usage, quit with !exit\n"


promptLoop :: InputT IO ()
promptLoop = getInputLine "GLaDOS> " >>= handleInput


handleInput :: Maybe String -> InputT IO ()
handleInput  Nothing     = outputStrLn "\nLeaving GLaDOS"
handleInput (Just input) = handleInput' $ trim input
  where
    handleInput' :: String -> InputT IO ()
    handleInput' ""       = promptLoop
    handleInput' "!help"  = printPromptHelp >> promptLoop
    handleInput' "!exit"  = outputStrLn "\nLeaving GLaDOS"
    handleInput' "!clear" = clearScreen >> promptLoop
    handleInput' text
      | "!ml" `isPrefixOf` text = multiLinePrompt $ drop 3 text
      | "!tokens "    `isPrefixOf` text =
          case getTokens (drop 8 text) of
            Left  err    -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right tokens -> printDebugTokens tokens >> promptLoop
      | "!blockexpr " `isPrefixOf` text =
          case getBlockExpr "Interpreter" (drop 11 text) of
            Left  err   -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right block -> printDebugBlockExpression block >> promptLoop
      | "!ast "       `isPrefixOf` text =
          case getAst "Interpreter" (drop 5 text) of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right ast -> printDebugAst ast >> promptLoop
      | "!bytecode "  `isPrefixOf` text = outputStrLn (drop 10 text) >> promptLoop
      | otherwise =
          case getAst "Interpreter" text of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right ast -> printDebugAst ast >> promptLoop


clearScreen :: InputT IO ()
clearScreen = outputStr "\ESC[H\ESC[2J"


multiLinePrompt :: String -> InputT IO ()
multiLinePrompt acc
  | "!end" `isSubsequenceOf` acc = handleInput $ Just $ take (length acc - 4) acc
  | acc `elem` ["!tokens", "!blockexpr", "!ast"] = multiLinePrompt $ acc ++ " "
  | otherwise = getInputLine "" >>= multiLinePrompt'
  where
    multiLinePrompt' :: Maybe String -> InputT IO ()
    multiLinePrompt'  Nothing     = outputStrLn "\nLeaving GLaDOS"
    multiLinePrompt' (Just input) = multiLinePrompt $ acc ++ input
