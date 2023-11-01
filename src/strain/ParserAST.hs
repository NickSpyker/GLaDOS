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
  | Module   String [Ast]
  | Section  [Ast]
  | Value    Literal
  | Binding  AstBinding
  | Operator Operator Ast Ast
  | Control  Control
  deriving (Show, Eq)


buildASTtree :: BExpr -> Either String Ast
buildASTtree (BModule name es) =
  case getAst es of
    Left  err -> Left err
    Right ast -> Right $ Module name ast
buildASTtree e = Left $ "<expect module but got> #" ++ show e ++ "#"


getAst :: [BExpr] -> Either String [Ast]
getAst = parseAST []


singleAst :: BExpr -> Maybe Ast
singleAst expr =
  case getAst [expr] of
    Right [ast] -> Just ast
    _           -> Nothing


parseAST :: [Ast] -> [BExpr] -> Either String [Ast]
parseAST _ (BModule name _ : _) = Left $ "<cannot have module inside other module> #" ++ name ++ "#"
parseAST accu [] = Right accu
parseAST accu ess = tryToParse accu ess parsers
  where
    tryToParse :: [Ast] -> [BExpr] -> [AstParser] -> Either String [Ast]
    tryToParse acc es [] = Left $ "<unmanaged situation>\n  #\n    acc = {\n      " ++ show acc ++ "\n    }\n    exprs = {\n      " ++ show es ++ "\n    }\n  #"
    tryToParse acc es (p : ps) =
      case p acc es of
        Nothing -> tryToParse acc es ps
        Just (new_ast, next) -> parseAST new_ast next

    parsers :: [AstParser]
    parsers =
      [ parseLiteral
      , parseNumberOperation
      ]


type AstParser = [Ast] -> [BExpr] -> Maybe ([Ast], [BExpr])


parseLiteral :: AstParser
parseLiteral _   (T (TokLit   _) : T (TokLit _) : _) = Nothing
parseLiteral acc (T (TokLit lit) : next) = Just (acc ++ [Value lit], next)
parseLiteral _ _ = Nothing


parseNumberOperation :: AstParser
parseNumberOperation [] _ = Nothing
parseNumberOperation acc (T op : T y : T mop : next)
  | isPriorOp op && isPriorOp mop =
      case parseNumberOperation acc [T op, T y] of
        Just (new, []) -> parseNumberOperation new (T mop : next)
        _ -> Nothing
  | not (isPriorOp op) && isPriorOp mop =
      case singleAst (T y) of
        Nothing -> Nothing
        Just yv ->
          case parseNumberOperation [yv] (T mop : next) of
            Just ([zv], new_next) ->
              case op of
                TokAdd -> Just (init acc ++ [Operator Add (last acc) zv], new_next)
                TokSub -> Just (init acc ++ [Operator Sub (last acc) zv], new_next)
                TokMul -> Just (init acc ++ [Operator Mul (last acc) zv], new_next)
                TokDiv -> Just (init acc ++ [Operator Div (last acc) zv], new_next)
                TokMod -> Just (init acc ++ [Operator Mod (last acc) zv], new_next)
                _      -> Nothing
            _ -> Nothing
  | otherwise =
      case parseNumberOperation acc [T op, T y] of
        Just (new, []) -> Just (new, T mop : next)
        _ -> Nothing
  where
    isPriorOp :: Token -> Bool
    isPriorOp o = o `elem` [TokMul, TokDiv, TokMod]
parseNumberOperation acc (T op : T y : next) =
  case singleAst (T y) of
    Nothing -> Nothing
    Just yv ->
      case op of
        TokAdd -> Just (init acc ++ [Operator Add (last acc) yv], next)
        TokSub -> Just (init acc ++ [Operator Sub (last acc) yv], next)
        TokMul -> Just (init acc ++ [Operator Mul (last acc) yv], next)
        TokDiv -> Just (init acc ++ [Operator Div (last acc) yv], next)
        TokMod -> Just (init acc ++ [Operator Mod (last acc) yv], next)
        _      -> Nothing
parseNumberOperation _ _ = Nothing
