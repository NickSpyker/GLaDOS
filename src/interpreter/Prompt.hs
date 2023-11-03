module Prompt (launchPrompt) where


import System.Console.Haskeline (runInputT, defaultSettings, historyFile, autoAddHistory, InputT, getInputLine, outputStrLn, outputStr)
import DebugOutput (printDebugTokens, printDebugBlockExpression, printDebugAst, printDebugByteCodes)
import Strain (getTokens, getBlockExpr, getAst, getByteCodes)
import PromptUsage (printPromptHelp)
import Data.Version (showVersion)
import Paths_glados (version)
import Data.List (isPrefixOf)
import Lib (trim, finishWith)
import Instruction (Prog)
import System.Info (os)


launchPrompt :: Prog -> IO ()
launchPrompt p = printHeader >> runInputT
  defaultSettings { historyFile = Just ".glados_history", autoAddHistory = True }
  (promptLoop p)


printHeader :: IO ()
printHeader = putStrLn $
  "GLaDOS Interpreter\nVersion "
    ++ showVersion version
    ++ " on "
    ++ os
    ++ "\n\nType !help for usage, quit with !exit\n"


promptLoop :: Prog -> InputT IO ()
promptLoop p = getInputLine "GLaDOS> " >>= handleInput
  where
    handleInput :: Maybe String -> InputT IO ()
    handleInput  Nothing     = outputStrLn "\nLeaving GLaDOS"
    handleInput (Just input) = handleInput' $ trim input

    handleInput' :: String -> InputT IO ()
    handleInput' ""       = promptLoop p
    handleInput' "!help"  = printPromptHelp >> promptLoop p
    handleInput' "!exit"  = outputStrLn "\nLeaving GLaDOS"
    handleInput' "!clear" = clearScreen >> promptLoop p
    handleInput' text
      | "!ml" `isPrefixOf` text = multiLinePrompt $ drop 3 text
      | "!tokens "    `isPrefixOf` text =
          case getTokens (drop 8 text) of
            Left  err    -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop p
            Right tokens -> printDebugTokens tokens >> promptLoop p
      | "!blockexpr " `isPrefixOf` text =
          case getBlockExpr "Interpreter" (drop 11 text) of
            Left  err   -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop p
            Right block -> printDebugBlockExpression block >> promptLoop p
      | "!ast "       `isPrefixOf` text =
          case getAst "Interpreter" (drop 5 text) of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop p
            Right ast -> printDebugAst ast >> promptLoop p
      | "!bytecode "  `isPrefixOf` text =
          case getByteCodes p ["Interpreter"] [drop 10 text] of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop p
            Right byt -> printDebugByteCodes byt >> promptLoop p
      | otherwise =
          case getByteCodes p ["Interpreter"] [text] of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop p
            Right byt -> printDebugByteCodes byt >> promptLoop byt
    
    multiLinePrompt :: String -> InputT IO ()
    multiLinePrompt acc
      | acc `finishWith` "!end" = handleInput $ Just $ take (length acc - 4) acc
      | acc `elem` ["!tokens", "!blockexpr", "!ast"] = multiLinePrompt $ acc ++ " "
      | otherwise = getInputLine "" >>= multiLinePrompt'
      where
        multiLinePrompt' :: Maybe String -> InputT IO ()
        multiLinePrompt'  Nothing     = outputStrLn "\nLeaving GLaDOS"
        multiLinePrompt' (Just input) = multiLinePrompt $ acc ++ input


clearScreen :: InputT IO ()
clearScreen = outputStr "\ESC[H\ESC[2J"
