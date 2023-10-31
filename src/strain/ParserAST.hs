module ParserAST (buildASTtree, Ast(..)) where


import Lexer (Token(..))
import BlockExpr (BExpr(..))
import Data (Literal(..))


data AstType
  = TyString
  | TyFloat
  | TyBool
  | TyChar
  | TyInt
  | TyArray AstType
  deriving (Show, Eq)


data AstBinding
  = Bound    String AstType (Maybe Ast)
  | Enum     String [AstBinding]
  | Struct   String [AstBinding]
  | Function String [AstBinding] AstType Ast
  deriving (Show, Eq)


data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | And
  | Or
  | Gt
  | Lt
  | Eq
  deriving (Show, Eq)


data Control
  = If Ast Ast Ast
  | While Ast Ast
  | Return Ast
  | For AstBinding Ast
  deriving (Show, Eq)


data Ast
  = Program  [Ast]
  | Value    Literal
  | Module   String Ast
  | Section  [Ast]
  | Binding  AstBinding
  | Operator Operator [Ast]
  | Control  Control
  | None
  deriving (Show, Eq)


buildASTtree :: BExpr -> Either String Ast
buildASTtree (BEProgram exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right (Section ast) -> Right $ Program ast
    Right ast -> Right $ Program [ast]
buildASTtree (BEModule name exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right ast -> Right $ Module name ast
buildASTtree (BEInBraces exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right ast -> Right ast
buildASTtree (BEInPrths exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right ast -> Right ast
buildASTtree (BEInHooks exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right ast -> Right ast
buildASTtree (BESection exprs) =
  case parseAST [] exprs of
    Left  err -> Left err
    Right ast -> Right ast
buildASTtree (BEExpr (TokLit lit))  = Right $ Value lit
buildASTtree (BEExpr (TokIde name)) = Left  $ "<Identifier without action> #" ++ name ++ "#"
buildASTtree (BEExpr expr)          = Left  $ "<Misplaced item> #" ++ show expr ++ "#"


parseAST :: [Ast] -> [BExpr] -> Either String Ast
parseAST acc [] = Right $ Section acc
parseAST acc (expr : next) =
  case buildASTtree expr of
    Left  err -> Left err
    Right ast -> parseAST (acc ++ [ast]) next
