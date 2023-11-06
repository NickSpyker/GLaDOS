module DebugOutput (printDebugTokens, printDebugBlockExpression, printDebugAst, printDebugByteCodes, printDebugProg, printDebugEnv) where


import System.Console.Haskeline (outputStr, outputStrLn, InputT)
import Instruction (Prog, Insts)
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
printDebugBlockExpression (BModule name bexprs) = outputStr ("\nBlock Expression (Module \"" ++ name ++ "\"):\n  ")
  >> printDebugBlockExpression'  bexprs  >> outputStrLn "\n"
printDebugBlockExpression bexprs                         = outputStr "\nBlock Expression:\n  "
  >> printDebugBlockExpression' [bexprs] >> outputStrLn "\n"

printDebugBlockExpression' :: [BExpr] -> InputT IO ()
printDebugBlockExpression' []          = return ()
printDebugBlockExpression' [br]        = outputStr $ show br
printDebugBlockExpression' (br : next) = printDebugBlockExpression' [br] >> outputStr " ↦ " >> printDebugBlockExpression' next


printDebugAst :: Ast -> InputT IO ()
printDebugAst ast = outputStrLn $ "\nAST tree:\n  " ++ show ast ++ "\n"


printDebugByteCodes :: Prog -> InputT IO ()
printDebugByteCodes [] = return ()
printDebugByteCodes [(mn, insts)] =
  outputStrLn ("\nByteCodes (Module \"" ++ mn ++ "\"):\n  " ++ show insts ++ "\n")
printDebugByteCodes (_ : ps) = printDebugByteCodes ps


printDebugProg :: (Insts, Prog) -> InputT IO ()
printDebugProg (main, prog) =
  outputStrLn ("\nProgram:\n  " ++ show prog)
    >> outputStrLn ("\nMain:\n  " ++ show main ++ "\n")


printDebugEnv :: [(String, Insts)] -> InputT IO ()
printDebugEnv [] = return ()
printDebugEnv ((name, code) : next) =
  outputStrLn ("\nModule \"" ++ name ++ "\":\n  " ++ show code ++ "\n")
  >> printDebugEnv next
