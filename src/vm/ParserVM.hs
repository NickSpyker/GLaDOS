module ParserVM (toByteCodes) where


import ParserAST (AstType(..), AstBinding(..), Operator(..), Control(..), Ast(..))
import Instruction (Data(..), Instruction(..), Args, Stack, Insts, Env, Prog)
import Data (Literal(..))


toByteCodes :: Ast -> Either String Prog
toByteCodes = toByteCodes' []
  where
    toByteCodes' :: Prog -> Ast -> Either String Prog
    toByteCodes' acc (Program []) = Right acc
    toByteCodes' acc (Program ((Module moduleName ast) : moduleNext)) =
      case buildByteCodes [] moduleName ast of
        Left err -> Left err
        Right by -> toByteCodes' (acc ++ by) $ Program moduleNext
    toByteCodes' _ wrongAst = Left $ "<expected program of modules, but got> #" ++ show wrongAst ++ "#"


type BcParser = [Ast] -> Maybe (Instruction, [Ast])


buildByteCodes :: Insts -> String -> [Ast] -> Either String Prog
buildByteCodes acc name [] = Right [(name, acc)]
buildByteCodes acc name (Value (LitInt n) : next) = buildByteCodes (acc ++ [Push $ Int n]) name next
buildByteCodes _ _ ast = Left $ show ast
