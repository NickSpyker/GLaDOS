module BlockExpr (BExpr(..), tokensToBlock) where


import Lexer (Token(..))


data BExpr
  = BEProgram  [BExpr]         -- all files, a list of BModules
  | BEModule    String [BExpr] -- one file, filename and code
  | BEInBraces [BExpr]         -- { this }
  | BEInPrths  [BExpr]         -- ( this )
  | BEInHooks  [BExpr]         -- [ this ]
  | BESection  [BExpr]         -- BIDING_OR_CALL_TOKEN this ;
  | BEExpr      Token          -- anything
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
parserBehavior ls mn [] = Right (BEModule mn ls, [])
----------------------------
-- Behavior of curly bracket
parserBehavior acc mn (TokOpCrlBr : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (BEInBraces bexpr, next) -> parserBehavior (acc ++ [BEInBraces bexpr]) mn next
    _ -> Left $ "<Expected '}'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClCrlBr : tokens) = Right (BEInBraces acc, tokens)
--------------------------
-- Behavior of parentheses
parserBehavior acc mn (TokOpPrth : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (BEInPrths bexpr, next) -> parserBehavior (acc ++ [BEInPrths bexpr]) mn next
    _ -> Left $ "<Expected ')'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClPrth : tokens) = Right (BEInPrths acc, tokens)
--------------------
-- Behavior of hooks
parserBehavior acc mn (TokOpCrch : tokens) =
  case parserBehavior [] mn tokens of
    Left err -> Left err
    Right (BEInHooks bexpr, next) -> parserBehavior (acc ++ [BEInHooks bexpr]) mn next
    _ -> Left $ "<Expected ']'> #" ++ show tokens ++ "#"
parserBehavior acc _ (TokClCrch : tokens) = Right (BEInHooks acc, tokens)
------------------------
-- Behavior of semicolon
parserBehavior acc mn (TokSmCol : tokens) = parserBehavior (handleSection acc) mn tokens
------------
-- Otherwise
parserBehavior acc mn (token : tokens) = parserBehavior (acc ++ [BEExpr token]) mn tokens


handleSection :: [BExpr] -> [BExpr]
handleSection acc = take (length acc - length (keep acc)) acc ++ [BESection (keep acc)]
  where
    keep :: [BExpr] -> [BExpr]
    keep i = reverse $ fetchToKeep $ reverse i

    fetchToKeep :: [BExpr] -> [BExpr]
    fetchToKeep [] = []
    fetchToKeep (BESection   _ : _) = []
    fetchToKeep (BEInBraces  _ : _) = []
    fetchToKeep (BEExpr TokVar : _) = [BEExpr TokVar]
    fetchToKeep (expr : next)       = expr : fetchToKeep next
