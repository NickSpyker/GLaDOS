module Prompt (launchPrompt) where


import System.Console.Haskeline (runInputT, defaultSettings, historyFile, autoAddHistory, InputT, getInputLine, outputStrLn, outputStr)
import DebugOutput (printDebugTokens, printDebugBlockExpression, printDebugAst, printDebugByteCodes, printDebugProg)
import Strain (getTokens, getBlockExpr, getAst, getByteCodes, joinBytecode, execute)
import Lib (trim, finishWith, nbrToFormatString)
import PreExecution (managesEntryPoint)
import Control.Monad.IO.Class (liftIO)
import PromptUsage (printPromptHelp)
import Data.Version (showVersion)
import Paths_glados (version)
import Data.List (isPrefixOf)
import Instruction (Prog)
import System.Info (os)
import VM (getNewProg)


launchPrompt :: Prog -> IO ()
launchPrompt p = printHeader >> runInputT
  defaultSettings { historyFile = Just ".glados_history", autoAddHistory = True }
  (promptLoop 0 p)


printHeader :: IO ()
printHeader = putStrLn $
  "GλaDOS Interpreter\nVersion "
    ++ showVersion version
    ++ " on "
    ++ os
    ++ "\n\nType !help for usage, quit with !exit\n"


promptLoop :: Int -> Prog -> InputT IO ()
promptLoop i p = getInputLine ("GλaDOS/" ++ nbrToFormatString i ++ "> ") >>= handleInput
  where
    handleInput :: Maybe String -> InputT IO ()
    handleInput  Nothing     = outputStrLn "\nLeaving GLaDOS"
    handleInput (Just input) = handleInput' $ trim input

    handleInput' :: String -> InputT IO ()
    handleInput' ""       = promptLoop (i + 1) p
    handleInput' "!help"  = printPromptHelp >> promptLoop (i + 1) p
    handleInput' "!exit"  = outputStrLn "\nLeaving GLaDOS"
    handleInput' "!clear" = clearScreen >> promptLoop (i + 1) p
    handleInput' text
      | "!ml" `isPrefixOf` text = multiLinePrompt $ drop 3 text
      | "!tokens "    `isPrefixOf` text =
          case getTokens (drop 8 text) of
            Left  err    -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop (i + 1) p
            Right tokens -> printDebugTokens tokens >> promptLoop (i + 1) p
      | "!blockexpr " `isPrefixOf` text =
          case getBlockExpr ("InterpreterLine" ++ show i) (drop 11 text) of
            Left  err   -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop (i + 1) p
            Right block -> printDebugBlockExpression block >> promptLoop (i + 1) p
      | "!ast "       `isPrefixOf` text =
          case getAst ("InterpreterLine" ++ show i) (drop 5 text) of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop (i + 1) p
            Right ast -> printDebugAst ast >> promptLoop (i + 1) p
      | "!bytecode "  `isPrefixOf` text =
          case getByteCodes p ["InterpreterLine" ++ show i] [drop 10 text] of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop i p
            Right byt -> printDebugByteCodes byt >> promptLoop (i + 1) p
      | "!prog " `isPrefixOf` text =
          case getByteCodes p ["InterpreterLine" ++ show i] [drop 6 text] of
            Left  err -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop i p
            Right prr -> printDebugProg (managesEntryPoint prr) >> promptLoop (i + 1) p
      | otherwise =
          case joinBytecode p ["InterpreterLine" ++ show i] [text] of
            Left  err  -> outputStrLn ("\nError:\n  " ++ err ++ "\n") >> promptLoop i p
            Right newP ->
              liftIO (execute p ["InterpreterLine" ++ show i] [text])
              >> promptLoop (i + 1) (getNewProg newP)
    

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
