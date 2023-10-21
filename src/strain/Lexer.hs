module Lexer (Token(..), tokenize) where


import Lib (isNumber, isNumberOrDot, extractBtwQuot, trim)
import Data.Char (isAlphaNum)
import Data (Literal(..))


data Token
  = OpPrth      -- '('
  | ClPrth      -- ')'
  | OpCrlBr     -- '{'
  | ClCrlBr     -- '}'
  | OpCrch      -- '['
  | ClCrch      -- ']
  | SmCol       -- ';'
  | Col         -- ':'
  | Dot         -- '.'
  | Com         -- ','
  | BckSlsh     -- '\'
  | Add         -- '+'
  | Sub         -- '-'
  | Mul         -- '*'
  | Div         -- '/'
  | Mod         -- '%'
  | Not         -- '!', "not"
  | Asgn        -- '='
  | And         -- "and"
  | Or          -- "or"
  | Fun         -- "fun"
  | Var         -- "let"
  | Gt          -- '>'
  | GtEq        -- ">="
  | Lt          -- '<'
  | LtEq        -- "<="
  | Eq          -- "=="
  | RetTy       -- "->"
  | For         -- "for"
  | If          -- "if"
  | Else        -- "else"
  | In          -- "in"
  | Loop        -- "loop"
  | While       -- "while"
  | Break       -- "break"
  | Return      -- "return"
  | Continue    -- "continue"
  | TyString    -- "string"
  | TyFloat     -- "float"
  | TyChar      -- "char"
  | TyInt       -- "int"
  | Lit Literal -- 0-9 "*" 0-9. '*'
  | Ide String  -- Identifiant name
  deriving (Show, Eq)


type ParserLexer = String -> Maybe (Token, String)


tokenize :: String -> Either String [Token]
tokenize buffer = tokenize' [
    parseBasicToken,
    parseWordToken,
    parseLitToken,
    parseIdeToken
  ]
  [] $ trim buffer
  where
    tokenize' :: [ParserLexer] -> [Token] -> String -> Either String [Token]
    tokenize' _ acc [] = Right acc
    tokenize' parsers acc input =
      case parseAllToken parsers input of
        Just (t, n) -> tokenize' parsers (acc ++ [t]) n
        Nothing     -> Left $ "Invalid tokens #" ++ input ++ "#"


parseAllToken :: [ParserLexer] -> String -> Maybe (Token, String)
parseAllToken _ [] = Nothing
parseAllToken [] _ = Nothing
parseAllToken (parser : next) input
  | head input `elem` [' ', '\t', '\r', '\n'] = parseAllToken (parser : next) $ tail input
  | otherwise =
      case parser input of
        Just (t, n) -> Just (t, n)
        Nothing     -> parseAllToken next input


parseBasicToken :: String -> Maybe (Token, String)
parseBasicToken ('+'  : next) = Just (Add, next)
parseBasicToken ('-'  : '>' : next) = Just (RetTy, next)
parseBasicToken ('-'  : next) = Just (Sub, next)
parseBasicToken ('*'  : next) = Just (Mul, next)
parseBasicToken ('/'  : next) = Just (Div, next)
parseBasicToken ('%'  : next) = Just (Mod, next)
parseBasicToken ('!'  : next) = Just (Not, next)
parseBasicToken ('('  : next) = Just (OpPrth,  next)
parseBasicToken (')'  : next) = Just (ClPrth,  next)
parseBasicToken ('{'  : next) = Just (OpCrlBr, next)
parseBasicToken ('}'  : next) = Just (ClCrlBr, next)
parseBasicToken ('['  : next) = Just (OpCrch,  next)
parseBasicToken (']'  : next) = Just (ClCrch,  next)
parseBasicToken (';'  : next) = Just (SmCol,   next)
parseBasicToken (':'  : next) = Just (Col,     next)
parseBasicToken ('.'  : next) = Just (Dot,     next)
parseBasicToken (','  : next) = Just (Com,     next)
parseBasicToken ('\\' : next) = Just (BckSlsh, next)
parseBasicToken ('>'  : '=' : next) = Just (GtEq, next)
parseBasicToken ('<'  : '=' : next) = Just (LtEq, next)
parseBasicToken ('>'  :       next) = Just (Gt,   next)
parseBasicToken ('<'  :       next) = Just (Lt,   next)
parseBasicToken ('='  : '=' : next) = Just (Eq,   next)
parseBasicToken ('='  :       next) = Just (Asgn, next)
parseBasicToken _ = Nothing


parseWordToken :: String -> Maybe (Token, String)
parseWordToken ('i' : 'f' :             next) = Just (If,   next)
parseWordToken ('o' : 'r' :             next) = Just (Or,   next)
parseWordToken ('f' : 'o' : 'r' :       next) = Just (For,  next)
parseWordToken ('n' : 'o' : 't' :       next) = Just (Not,  next)
parseWordToken ('a' : 'n' : 'd' :       next) = Just (And,  next)
parseWordToken ('f' : 'u' : 'n' :       next) = Just (Fun,  next)
parseWordToken ('l' : 'e' : 't' :       next) = Just (Var,  next)
parseWordToken ('e' : 'l' : 's' : 'e' : next) = Just (Else, next)
parseWordToken ('l' : 'o' : 'o' : 'p' :             next) = Just (Loop,     next)
parseWordToken ('w' : 'h' : 'i' : 'l' : 'e' :       next) = Just (While,    next)
parseWordToken ('b' : 'r' : 'e' : 'a' : 'k' :       next) = Just (Break,    next)
parseWordToken ('r' : 'e' : 't' : 'u' : 'r' : 'n' : next) = Just (Return,   next)
parseWordToken ('s' : 't' : 'r' : 'i' : 'n' : 'g' : next) = Just (TyString, next)
parseWordToken ('f' : 'l' : 'o' : 'a' : 't' :       next) = Just (TyFloat,  next)
parseWordToken ('c' : 'h' : 'a' : 'r' :             next) = Just (TyChar,   next)
parseWordToken ('i' : 'n' : 't' :                   next) = Just (TyInt,    next)
parseWordToken ('i' : 'n' : next) = Just (In, next)
parseWordToken _ = Nothing


parseLitToken :: String -> Maybe (Token, String)
parseLitToken = parseLitToken' []
  where
    parseLitToken' :: String -> String -> Maybe (Token, String)
    parseLitToken' []  [] = Nothing
    parseLitToken' acc []
      | isNumber acc      = Just (Lit (LitInt   (read acc)), [])
      | isNumberOrDot acc = Just (Lit (LitFloat (read acc)), [])
      | otherwise         = Nothing
    parseLitToken' [] ('\'' : '\\' : c : '\'' : next) = Just (Lit (LitSChar c), next)
    parseLitToken' [] ('\'' :        c : '\'' : next) = Just (Lit (LitChar  c), next)
    parseLitToken' [] ('"' : next) =
      case extractBtwQuot ('"' : next) of
        Just str -> Just (Lit (LitString str), drop (length str + 2) next)
        Nothing  -> Nothing
    parseLitToken' acc (c : next)
      | isNumberOrDot [c] && isNumberOrDot acc = parseLitToken' (c : acc) next
      | isNumber acc      = Just (Lit (LitInt   (read acc)), next)
      | isNumberOrDot acc = Just (Lit (LitFloat (read acc)), next)
      | otherwise         = Nothing


parseIdeToken :: String -> Maybe (Token, String)
parseIdeToken [] = Nothing
parseIdeToken (c : next)
  | isAlphaNum c = parseIdeToken' [] (c : next)
  | otherwise = Nothing
  where
    parseIdeToken' :: String -> String -> Maybe (Token, String)
    parseIdeToken' [] [] = Nothing
    parseIdeToken' acc [] = Just (Ide acc, [])
    parseIdeToken' acc ('_' : n) = parseIdeToken' (acc ++ ['_']) n
    parseIdeToken' acc ( cb : n)
      | isAlphaNum cb = parseIdeToken' (acc ++ [cb]) n
      | otherwise     = Just (Ide acc, cb : n)
