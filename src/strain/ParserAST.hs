module ParserAST (buildASTtree, Ast(..)) where


import BlockExpr (BExpr(..))
import Data (Literal(..))
import Lexer (Token(..))


data AstType
  = TyString
  | TyFloat
  | TyBool
  | TyChar
  | TyInt
  | TyCustom String
  | TyArray  AstType
  deriving (Show, Eq)

data AstBinding
  = Bound    String AstType Ast
  | Enum     String [AstBinding]
  | Struct   String [AstBinding]
  | Function String [AstBinding] AstType Ast
  deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Mod | Not | And | Or | Gt | Lt | Eq deriving (Show, Eq)

data Control
  = If     Ast Ast Ast
  | While  Ast Ast
  | Return Ast
  | For    AstBinding Ast Ast Ast
  deriving (Show, Eq)

data Ast
  = Program  [Ast]
  | Module   String Ast
  | Section  [Ast]
  | Value    Literal
  | Binding  AstBinding
  | Operator Operator Ast Ast
  | Control  Control
  deriving (Show, Eq)


buildASTtree :: BExpr -> Either String Ast
buildASTtree _ = Right $ Program []
