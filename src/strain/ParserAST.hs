module ParserAST (buildASTtree, Ast(..)) where


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
buildASTtree _ = Right None
