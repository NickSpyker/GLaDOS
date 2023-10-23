module BlockExpr (BExpr(..), tokensToBlock) where


import Lexer (Token(..))


data BExpr
  = Program  [BExpr]          -- all files, a list of modules
  | Module    String [BExpr]  -- one file, filename and code
  | InBraces [BExpr]          -- { this }
  | InPrths  [BExpr]          -- ( this )
  | InHooks  [BExpr]          -- [ this ]
  | BefSmCol [BExpr]          -- BIDING_OR_CALL_TOKEN this ;
  | Expr      Token           -- anything
  deriving (Show, Eq)


type ParserBlockExpr = [Token] -> Maybe (BExpr, [Token])


tokensToBlock :: String -> [Token] -> Either String BExpr
tokensToBlock moduleName tokens =
  case tokensToBlock' tokens of
    Left  err  -> Left err
    Right list -> Right $ Module moduleName list
  where
    tokensToBlock' :: [Token] -> Either String [BExpr]
    tokensToBlock' tokens = Left "no"
