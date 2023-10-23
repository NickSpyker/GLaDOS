module BlockExpr (BExpr(..), tokensToBlock) where


import Lexer (Token(..))


data BExpr
  = Program  [BExpr]         -- all files, a list of modules
  | Module    String [BExpr] -- one file, filename and code
  | InBraces [BExpr]         -- { this }
  | InPrths  [BExpr]         -- ( this )
  | InHooks  [BExpr]         -- [ this ]
  | Section  [BExpr]         -- BIDING_OR_CALL_TOKEN this ;
  | Expr      Token          -- anything
  deriving (Show, Eq)


tokensToBlock :: String -> [Token] -> Either String BExpr
tokensToBlock = tokensToBlock'
  where
    tokensToBlock' :: String -> [Token] -> Either String BExpr
    tokensToBlock' mn tks =
      case parserBehavior [] mn tks of
        Left err          -> Left err
        Right (bexpr, []) -> Right bexpr
        Right (_, mi)     -> Left $ "<Missing section> #" ++ show mi ++ "#"


parserBehavior :: [BExpr] -> String -> [Token] -> Either String (BExpr, [Token])
parserBehavior [expr] _ [] = Right (expr, [])
parserBehavior [] _ [] = Left "<Invalid end of file> ##"
parserBehavior ls mn [] = Right (Module mn ls, [])
----------------------------
-- Behavior of curly bracket
parserBehavior acc mn (OpCrlBr : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (InBraces bexpr, next) -> parserBehavior (acc ++ [InBraces bexpr]) mn next
    _ -> Left $ "<Expected '}'> #" ++ show tokens ++ "#"
parserBehavior acc _ (ClCrlBr : tokens) = Right (InBraces acc, tokens)
--------------------------
-- Behavior of parentheses
parserBehavior acc mn (OpPrth : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (InPrths bexpr, next) -> parserBehavior (acc ++ [InPrths bexpr]) mn next
    _ -> Left $ "<Expected ')'> #" ++ show tokens ++ "#"
parserBehavior acc _ (ClPrth : tokens) = Right (InPrths acc, tokens)
--------------------
-- Behavior of hooks
parserBehavior acc mn (OpCrch : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (InHooks bexpr, next) -> parserBehavior (acc ++ [InHooks bexpr]) mn next
    _ -> Left $ "<Expected ']'> #" ++ show tokens ++ "#"
parserBehavior acc _ (ClCrch : tokens) = Right (InHooks acc, tokens)
------------------------
-- Behavior of semicolon
parserBehavior acc _ (SmCol : tokens) = Right (Section acc, tokens)
parserBehavior acc mn (token : tokens)
  | token `elem` [Var, Const, Return, DeclType, Import] =
      case parserBehavior [] mn tokens of
        Left err -> Left err
        Right (Section bexpr, next) -> parserBehavior (acc ++ [Section (Expr token : bexpr)]) mn next
        _ -> Left $ "<Expected ';'> #" ++ show tokens ++ "#"
  | otherwise = parserBehavior (acc ++ [Expr token]) mn tokens
