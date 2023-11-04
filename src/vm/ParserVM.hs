module ParserVM (toByteCodes) where


import ParserAST (AstType(..), AstBinding(..), Operator(..), Control(..), Ast(..))
import Instruction (Data(..), Instruction(..), Args, Stack, Insts, Env, Prog)
import Data (Literal(..))


toByteCodes :: Ast -> Either String Prog
toByteCodes = toByteCodes' []
  where
    toByteCodes' :: Prog -> Ast -> Either String Prog
    toByteCodes' acc (Program []) = Right acc
    toByteCodes' acc (Program ((Module moduleName ast) : moduleNext)) =
      case buildByteCodes [] moduleName ast of
        Left err -> Left err
        Right by -> toByteCodes' (acc ++ by) $ Program moduleNext
    toByteCodes' _ wrongAst = Left $ "<expected program of modules, but got> #" ++ show wrongAst ++ "#"


buildByteCodes :: Insts -> String -> [Ast] -> Either String Prog
buildByteCodes acc name [ ] = Right [(name, acc)]
buildByteCodes acc name ast =
  case getBC ast of
    Just (i, n) -> buildByteCodes (acc ++ i) name n
    Nothing     -> Left $ "<invalid semantics>\n  #\n    bytecode = {\n      " ++ show acc ++ "\n    }\n    ast = {\n      " ++ show ast ++ "\n    }\n  #"


getBC :: BcParser
getBC = tryToParse parsers
  where
    tryToParse :: [BcParser] -> [Ast] -> Maybe (Insts, [Ast])
    tryToParse (p : ps) ast =
      case p ast of
        Nothing     -> tryToParse ps ast
        Just (i, n) -> Just (i, n)
    tryToParse _ _ = Nothing

    parsers :: [BcParser]
    parsers =
      [ parseLiteral
      , parseOperation
      ]


type BcParser = [Ast] -> Maybe (Insts, [Ast])


parseLiteral :: BcParser
parseLiteral (Value (LitString v) : next) = Just (Push ArrayStart : parseLiteral' v, next)
  where
    parseLiteral' :: [Char] -> Insts
    parseLiteral' [     ] = [Push ArrayEnd]
    parseLiteral' (c : n) = (Push (Char c) : parseLiteral' n)
parseLiteral (Value (LitSChar  v) : next) =
  case getC v of
    Just c  -> Just ([Push $ Char c], next)
    Nothing -> Nothing
  where
    getC :: Char -> Maybe Char
    getC char =
      case char of
      '0' -> Just '\0'
      'a' -> Just '\a'
      'b' -> Just '\b'
      't' -> Just '\t'
      'n' -> Just '\n'
      'v' -> Just '\v'
      'f' -> Just '\f'
      'r' -> Just '\r'
      '\'' -> Just '\''
      '"' -> Just '\"'
      _   -> Nothing
parseLiteral (Value (LitFloat  v) : next) = Just ([Push $ Float v], next)
parseLiteral (Value (LitArray  v) : next) =
  case parseLiteral' [] v of
    Nothing -> Nothing
    Just is -> Just (Push ArrayStart : is, next)
  where
    parseLiteral' :: Insts -> [Literal] -> Maybe Insts
    parseLiteral' acc [     ] = Just $ acc ++ [Push ArrayEnd]
    parseLiteral' acc (l : n) =
      case getBC [Value l] of
        Just (i, []) -> parseLiteral' (acc ++ i) n
        _            -> Nothing
parseLiteral (Value (LitChar   v) : next) = Just ([Push $ Char  v], next)
parseLiteral (Value (LitBool   v) : next) = Just ([Push $ Bool  v], next)
parseLiteral (Value (LitInt    v) : next) = Just ([Push $ Int   v], next)
parseLiteral _ = Nothing


parseOperation :: BcParser
parseOperation (Operator ParserAST.Not x _ : next) =
  case getBC [x] of
    Just (i, []) -> Just (i ++ [Push Instruction.Not, Call], next)
    _            -> Nothing
parseOperation (Operator op x y : next) =
  case (getBC [x], getBC [y]) of
    (Just (xi, []), Just (yi, [])) ->
      case op of
        ParserAST.Add -> Just (xi ++ yi ++ [Push Instruction.Add, Call], next)
        ParserAST.Sub -> Just (xi ++ yi ++ [Push Instruction.Sub, Call], next)
        ParserAST.Mul -> Just (xi ++ yi ++ [Push Instruction.Mul, Call], next)
        ParserAST.Div -> Just (xi ++ yi ++ [Push Instruction.Div, Call], next)
        ParserAST.Mod -> Just (xi ++ yi ++ [Push Instruction.Mod, Call], next)
        ParserAST.Or  -> Just (xi ++ yi ++ [Push  Instruction.Or, Call], next)
        ParserAST.And -> Just (xi ++ yi ++ [Push Instruction.And, Call], next)
        ParserAST.Gt  -> Just (xi ++ yi ++ [Push  Instruction.Gt, Call], next)
        ParserAST.Lt  -> Just (xi ++ yi ++ [Push  Instruction.Lt, Call], next)
        ParserAST.Eq  -> Just (xi ++ yi ++ [Push  Instruction.Eq, Call], next)
    _ -> Nothing
parseOperation _ = Nothing
