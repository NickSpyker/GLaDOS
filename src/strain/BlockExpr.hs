module BlockExpr where


import Lexer (Token(..))


data BExpr
  = InPrths  [Token]
  | InHooks  [Token]
  | InBraces [Token]
  | BefSmCol [Token]
  | Expr      Token
