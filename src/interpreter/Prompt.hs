module Prompt (launchPrompt) where


import System.Console.Haskeline (runInputT, defaultSettings, historyFile, autoAddHistory, InputT, getInputLine, outputStrLn)
import DebugOutput (printDebugTokens, printDebugBlockExpression)
import PromptUsage (printPromptHelp)
import Data.Version (showVersion)
import BlockExpr (tokensToBlock)
import Paths_glados (version)
import Data.List (isPrefixOf)
import BlockExpr (BExpr)
import System.Info (os)
import Lexer (tokenize)
import Lib (trim)


launchPrompt :: BExpr -> IO ()
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
handleInput  Nothing     = outputStrLn "\nLeaving GLaDOS" >> return ()
handleInput (Just input) = handleInput' $ trim input
  where
    handleInput' :: String -> InputT IO ()
    handleInput' "!help" = printPromptHelp >> promptLoop
    handleInput' "!exit" = outputStrLn "\nLeaving GLaDOS" >> return ()
    handleInput' text
      | "!tokens "    `isPrefixOf` text =
          case tokenize (drop 8 text) of
            Left  err    -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right tokens -> printDebugTokens tokens >> promptLoop
      | "!blockexpr " `isPrefixOf` text =
          case tokenize (drop 11 text) of
            Left  err    -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
            Right tokens ->
              case tokensToBlock "Interpreter" tokens of
                Left  err   -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop
                Right block -> printDebugBlockExpression block >> promptLoop
      | "!ast "       `isPrefixOf` text = outputStrLn (drop  5 text) >> promptLoop
      | "!bytecode "  `isPrefixOf` text = outputStrLn (drop 10 text) >> promptLoop
      | otherwise = outputStrLn text >> promptLoop
