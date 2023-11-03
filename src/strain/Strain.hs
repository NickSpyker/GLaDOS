module Strain (getTokens, getBlockExpr, getAst, getByteCodes) where


import ParserAST (buildASTtree, Ast(..))
import BlockExpr (tokensToBlock, BExpr)
import Lexer (tokenize, Token)
import ParserVM (toByteCodes)
import Instruction (Prog)


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


getByteCodes :: Prog -> [String] -> [String] -> Either String Prog
getByteCodes progAcc ps is =
  case buildAstProgram [] ps is of
    Left  err -> Left err
    Right pro ->
      case toByteCodes pro of
        Left  err -> Left err
        Right vpr -> Right $ progAcc ++ vpr
  where
    buildAstProgram :: [Ast] -> [String] -> [String] -> Either String Ast
    buildAstProgram acc _ [] = Right $ Program acc
    buildAstProgram acc [] _ = Right $ Program acc
    buildAstProgram acc (path : paths) (input : inputs) =
      case getAst path input of
        Left   err              -> Left err
        Right (Module name ast) -> buildAstProgram (acc ++ [Module name ast]) paths inputs
        Right  wrongAst         -> Left $ "<invalid ast output> #" ++ show wrongAst ++ "#"
