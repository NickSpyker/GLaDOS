module PromptUsage (printPromptHelp) where


import System.Console.Haskeline (InputT, outputStrLn)


printPromptHelp :: InputT IO ()
printPromptHelp =
     outputStrLn "\nUtility Commands:"
  >> outputStrLn "  !help       Show this message"
  >> outputStrLn "  !exit       Exit the prompt"
  >> outputStrLn "  !ml         Start an input on several lines"
  >> outputStrLn "  !end        Ends multi-line input"
  >> outputStrLn "\nDebug Commands:"
  >> outputStrLn "  !tokens     Show tokenization of the input"
  >> outputStrLn "  !blockexpr  Show block expression of the input"
  >> outputStrLn "  !ast        Show AST tree of the input"
  >> outputStrLn "  !bytecode   Show bytecode of the input\n"
