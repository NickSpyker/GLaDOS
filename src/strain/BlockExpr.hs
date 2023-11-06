module BlockExpr (BExpr(..), tokensToBlock, handleSection) where


import Lexer (Token(..))


data BExpr
  = BModule   String [BExpr] -- one file, filename and code
  | Braces   [BExpr]         -- { this }
  | Prths    [BExpr]         -- ( this )
  | Hooks    [BExpr]         -- [ this ]
  | BSection [BExpr]         -- BIDING_OR_CALL_TOKEN this ;
  | T         Token          -- anything
  deriving (Show, Eq)


tokensToBlock :: String -> [Token] -> Either String BExpr
tokensToBlock = tokensToBlock'
  where
    tokensToBlock' :: String -> [Token] -> Either String BExpr
    tokensToBlock' mn tks =
      case parserBehavior [] mn tks of
        Left  err         -> Left err
        Right (bexpr, []) -> Right bexpr
        Right (_, mi)     -> Left $ "<Missing section> #" ++ show mi ++ "#"


parserBehavior :: [BExpr] -> String -> [Token] -> Either String (BExpr, [Token])
parserBehavior [] _  [] = Left "<Invalid end of file> #@#"
parserBehavior ls mn [] = Right (BModule mn ls, [])
----------------------------
-- Behavior of curly bracket
parserBehavior acc mn (TokOpCrlBr : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (Braces bexpr, next) -> parserBehavior (acc ++ [Braces bexpr]) mn next
    _ -> Left $ "<Expected '}'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClCrlBr : tokens) = Right (Braces acc, tokens)
--------------------------
-- Behavior of parentheses
parserBehavior acc mn (TokOpPrth : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (Prths bexpr, next) -> parserBehavior (acc ++ [Prths bexpr]) mn next
    _ -> Left $ "<Expected ')'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClPrth : tokens) = Right (Prths acc, tokens)
--------------------
-- Behavior of hooks
parserBehavior acc mn (TokOpCrch : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (Hooks bexpr, next) -> parserBehavior (acc ++ [Hooks bexpr]) mn next
    _ -> Left $ "<Expected ']'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClCrch : tokens) = Right (Hooks acc, tokens)
------------------------
-- Behavior of semicolon
parserBehavior acc mn (TokSmCol : tokens) = parserBehavior (handleSection acc) mn tokens
------------
-- Otherwise
parserBehavior acc mn (token : tokens) = parserBehavior (acc ++ [T token]) mn tokens


handleSection :: [BExpr] -> [BExpr]
handleSection acc = take (length acc - length (keep acc)) acc ++ [BSection (keep acc)]
  where
    keep :: [BExpr] -> [BExpr]
    keep i = reverse $ fetchToKeep $ reverse i

    fetchToKeep :: [BExpr] -> [BExpr]
    fetchToKeep [] = []
    fetchToKeep (BSection   _ : _) = []
    fetchToKeep (Braces  _ : _) = []
    fetchToKeep (T TokVar : _) = [T TokVar]
    fetchToKeep (expr : next)       = expr : fetchToKeep next
