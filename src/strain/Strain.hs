module Strain (getTokens, getBlockExpr, getAst) where


import BlockExpr (tokensToBlock, BExpr)
import ParserAST (buildASTtree, Ast)
import Lexer (tokenize, Token)


getTokens :: String -> Either String [Token]
getTokens = tokenize


getBlockExpr :: String -> String -> Either String BExpr
getBlockExpr path input =
  case getTokens input of
    Left  err    -> Left err
    Right tokens -> tokensToBlock path tokens


getAst :: String -> String -> Either String Ast
getAst path input =
  case getBlockExpr path input of
    Left  err   -> Left err
    Right bexpr -> buildASTtree bexpr
