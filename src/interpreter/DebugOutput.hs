module DebugOutput (printDebugTokens, printDebugBlockExpression) where


import System.Console.Haskeline
import BlockExpr (BExpr(..))
import Lexer (Token(..))


printDebugTokens :: [Token] -> InputT IO ()
printDebugTokens tokens = outputStr "\nTokens:\n  " >> printDebugTokens' tokens >> outputStrLn ""
  where
    printDebugTokens' :: [Token] -> InputT IO ()
    printDebugTokens' []           = return ()
    printDebugTokens' [tok]        = outputStrLn $ show tok
    printDebugTokens' (tok : next) = outputStr (show tok ++ " ↦ ") >> printDebugTokens' next


printDebugBlockExpression :: BExpr -> InputT IO ()
printDebugBlockExpression (Program  bexprs) = outputStr "\nBlock Expression:\n  " >> printDebugBlockExpression'  bexprs  >> outputStrLn "\n"
printDebugBlockExpression (Module _ bexprs) = outputStr "\nBlock Expression:\n  " >> printDebugBlockExpression'  bexprs  >> outputStrLn "\n"
printDebugBlockExpression bexprs            = outputStr "\nBlock Expression:\n  " >> printDebugBlockExpression' [bexprs] >> outputStrLn "\n"

printDebugBlockExpression' :: [BExpr] -> InputT IO ()
printDebugBlockExpression' []          = return ()
printDebugBlockExpression' [br]        = outputStr $ show br
printDebugBlockExpression' (br : next) = printDebugBlockExpression' [br] >> outputStr " ↦ " >> printDebugBlockExpression' next
