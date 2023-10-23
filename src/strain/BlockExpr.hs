module BlockExpr (BExpr(..), tokensToBlock) where


import Lexer (Token(..))


data BExpr
  = Program  [BExpr]         -- all files, a list of modules
  | Module    String [BExpr] -- one file, filename and code
  | InBraces [BExpr]         -- { this }
  | InPrths  [BExpr]         -- ( this )
  | InHooks  [BExpr]         -- [ this ]
  | BefSmCol [BExpr]         -- BIDING_OR_CALL_TOKEN this ;
  | Expr      Token          -- anything
  | Eof
  deriving (Show, Eq)


tokensToBlock :: String -> [Token] -> Either String BExpr
tokensToBlock moduleName tokens =
  case tokensToBlock' tokens of
    Left  err  -> Left err
    Right list -> Right $ Module moduleName list
  where
    tokensToBlock' :: [Token] -> Either String [BExpr]
    tokensToBlock' tks =
      case parserBehavior [] tks of
        Left err          -> Left err
        Right (bexpr, []) -> Right [bexpr]
        Right (_, mi)     -> Left $ "<Missing section> #" ++ show mi ++ "#"


parserBehavior :: [BExpr] -> [Token] -> Either String (BExpr, [Token])
parserBehavior _ [] = Right (Eof, [])
----------------------------
-- Behavior of curly bracket
parserBehavior acc (OpCrlBr : tokens) =
  case parserBehavior [] tokens of
    Left err -> Left err
    Right (InBraces bexpr, next) -> parserBehavior (acc ++ [InBraces bexpr]) next
    _ -> Left $ "<Expected '}'> #" ++ show tokens ++ "#"
parserBehavior acc (ClCrlBr : tokens) = Right (InBraces acc, tokens)
--------------------------
-- Behavior of parentheses
parserBehavior acc (OpPrth : tokens) =
  case parserBehavior [] tokens of
    Left err -> Left err
    Right (InPrths bexpr, next) -> parserBehavior (acc ++ [InPrths bexpr]) next
    _ -> Left $ "<Expected ')'> #" ++ show tokens ++ "#"
parserBehavior acc (ClPrth : tokens) = Right (InPrths acc, tokens)
--------------------
-- Behavior of hooks
parserBehavior acc (OpCrch : tokens) =
  case parserBehavior [] tokens of
    Left err -> Left err
    Right (InHooks bexpr, next) -> parserBehavior (acc ++ [InHooks bexpr]) next
    _ -> Left $ "<Expected ']'> #" ++ show tokens ++ "#"
parserBehavior acc (ClCrch : tokens) = Right (InHooks acc, tokens)
------------------------
-- Behavior of semicolon
parserBehavior acc (SmCol : tokens) = Right (BefSmCol acc, tokens)
parserBehavior acc (token : tokens)
  | token `elem` [Var, Const, Return, DeclType, Import, RName] =
      case parserBehavior [] tokens of
        Left err -> Left err
        Right (BefSmCol bexpr, next) -> parserBehavior (acc ++ [BefSmCol bexpr]) next
        _ -> Left $ "<Expected ';'> #" ++ show tokens ++ "#"
  | otherwise = parserBehavior (acc ++ [Expr token]) tokens
