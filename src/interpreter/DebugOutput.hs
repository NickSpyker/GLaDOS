module DebugOutput (printDebugTokens, printDebugBlockExpression, printDebugAst) where


import System.Console.Haskeline (outputStr, outputStrLn, InputT)
import BlockExpr (BExpr(..))
import ParserAST (Ast(..))
import Lexer (Token(..))


printDebugTokens :: [Token] -> InputT IO ()
printDebugTokens tokens = outputStr "\nTokens:\n  " >> printDebugTokens' tokens >> outputStrLn ""
  where
    printDebugTokens' :: [Token] -> InputT IO ()
    printDebugTokens' []           = return ()
    printDebugTokens' [tok]        = outputStrLn $ show tok
    printDebugTokens' (tok : next) = outputStr (show tok ++ " ↦ ") >> printDebugTokens' next


printDebugBlockExpression :: BExpr -> InputT IO ()
printDebugBlockExpression (BEModule name bexprs) = outputStr ("\nBlock Expression (Module \"" ++ name ++ "\"):\n  ")
  >> printDebugBlockExpression'  bexprs  >> outputStrLn "\n"
printDebugBlockExpression bexprs                         = outputStr "\nBlock Expression:\n  "
  >> printDebugBlockExpression' [bexprs] >> outputStrLn "\n"

printDebugBlockExpression' :: [BExpr] -> InputT IO ()
printDebugBlockExpression' []          = return ()
printDebugBlockExpression' [br]        = outputStr $ show br
printDebugBlockExpression' (br : next) = printDebugBlockExpression' [br] >> outputStr " ↦ " >> printDebugBlockExpression' next


printDebugAst :: Ast -> InputT IO ()
printDebugAst ast = outputStrLn $ "\nAST tree:\n  " ++ show ast ++ "\n"
