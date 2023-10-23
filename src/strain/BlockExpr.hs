module BlockExpr (BExpr(..), tokensToBlock) where


import Lexer (Token(..))


data BExpr
  = Program  [BExpr]
  | InPrths  [BExpr]
  | InHooks  [BExpr]
  | InBraces [BExpr]
  | BefSmCol [BExpr]
  | Expr      Token
  | Block     String [BExpr]
  deriving (Show, Eq)


tokensToBlock :: String -> [Token] -> Either String BExpr
tokensToBlock moduleName tokens =
  case tokensToBlock' tokens of
    Left  err          -> Left err
    Right (Block _ bl) -> Right $ Block moduleName bl
    Right (Expr expr)  -> Right $ Block moduleName [Expr expr]
    Right block        -> Right $ Block moduleName [block]
  where
    tokensToBlock' :: [Token] -> Either String BExpr
    tokensToBlock' _ = Left "no"
