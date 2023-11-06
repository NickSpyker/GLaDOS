module Lexer (Token(..), tokenize) where


import Lib (isNumber, isNumberOrDot, extractBtwQuot, trim)
import Data.Char (isAlphaNum)
import Data (Literal(..))


data Token
  = TokOpPrth      -- '('
  | TokClPrth      -- ')'
  | TokOpCrlBr     -- '{'
  | TokClCrlBr     -- '}'
  | TokOpCrch      -- '['
  | TokClCrch      -- ']
  | TokSmCol       -- ';'
  | TokCol         -- ':'
  | TokDCol        -- "::"
  | TokDot         -- '.'
  | TokCom         -- ','
  | TokBckSlsh     -- '\'
  | TokAdd         -- '+'
  | TokSub         -- '-'
  | TokMul         -- '*'
  | TokDiv         -- '/'
  | TokMod         -- '%'
  | TokNot         -- '!', "not"
  | TokAsgn        -- '='
  | TokAnd         -- "and"
  | TokOr          -- "or"
  | TokFun         -- "fun"
  | TokVar         -- "let"
  | TokConst       -- "const"
  | TokGt          -- '>'
  | TokGtEq        -- ">="
  | TokLt          -- '<'
  | TokLtEq        -- "<="
  | TokEq          -- "=="
  | TokNotEq       -- "!="
  | TokAddAsgn     -- "+="
  | TokMulAsgn     -- "*="
  | TokSubAsgn     -- "-="
  | TokDivAsgn     -- "/="
  | TokRetTy       -- "->"
  | TokFor         -- "for"
  | TokIf          -- "if"
  | TokElse        -- "else"
  | TokIn          -- "in"
  | TokLoop        -- "loop"
  | TokWhile       -- "while"
  | TokBreak       -- "break"
  | TokReturn      -- "return"
  | TokContinue    -- "continue"
  | TokTyString    -- "string"
  | TokTyFloat     -- "float"
  | TokTyBool      -- "bool"
  | TokTyVoid      -- "void"
  | TokTyChar      -- "char"
  | TokTyInt       -- "int"
  | TokDeclType    -- "type"
  | TokStruct      -- "struct"
  | TokEnum        -- "enum"
  | TokImport      -- "use"
  | TokRName       -- "as"
  | TokLit Literal -- 0-9 "*" 0-9. '*'
  | TokIde String  -- Identifiant name
  deriving (Show, Eq)


type ParserLexer = String -> Maybe (Token, String)


tokenize :: String -> Either String [Token]
tokenize buffer = tokenize' parsers [] $ trim buffer
  where
    tokenize' :: [ParserLexer] -> [Token] -> String -> Either String [Token]
    tokenize' _ acc [] = Right acc
    tokenize' prs acc input =
      case parseAllToken prs input of
        Just (t, n) -> tokenize' prs (acc ++ [t]) n
        Nothing     -> Left $ "<Invalid token> #" ++ input ++ "#"
    
    parsers :: [ParserLexer]
    parsers =
      [ parseBasicToken
      , parseWordToken
      , parseLitToken
      , parseIdeToken
      ]


parseAllToken :: [ParserLexer] -> String -> Maybe (Token, String)
parseAllToken _ [] = Nothing
parseAllToken [] _ = Nothing
parseAllToken parsers ('/' : '/' : input) = parseAllToken parsers $ ignoreComment input
  where
    ignoreComment :: String -> String
    ignoreComment ('\n' : next) = next
    ignoreComment (_    : next) = ignoreComment next
    ignoreComment _ = ""
parseAllToken (parser : next) input
  | head input `elem` [' ', '\t', '\r', '\n'] = parseAllToken (parser : next) $ tail input
  | otherwise =
      case parser input of
        Just (t, n) -> Just (t, n)
        Nothing     -> parseAllToken next input


parseBasicToken :: ParserLexer
parseBasicToken ('%'  : next) = Just (TokMod, next)
parseBasicToken ('('  : next) = Just (TokOpPrth,  next)
parseBasicToken (')'  : next) = Just (TokClPrth,  next)
parseBasicToken ('{'  : next) = Just (TokOpCrlBr, next)
parseBasicToken ('}'  : next) = Just (TokClCrlBr, next)
parseBasicToken ('['  : next) = Just (TokOpCrch,  next)
parseBasicToken (']'  : next) = Just (TokClCrch,  next)
parseBasicToken (';'  : next) = Just (TokSmCol,   next)
parseBasicToken ('.'  : next) = Just (TokDot,     next)
parseBasicToken (','  : next) = Just (TokCom,     next)
parseBasicToken ('\\' : next) = Just (TokBckSlsh, next)
parseBasicToken (':'  : ':' : next) = Just (TokDCol,    next)
parseBasicToken ('-'  : '>' : next) = Just (TokRetTy,   next)
parseBasicToken ('>'  : '=' : next) = Just (TokGtEq,    next)
parseBasicToken ('<'  : '=' : next) = Just (TokLtEq,    next)
parseBasicToken ('!'  : '=' : next) = Just (TokNotEq,   next)
parseBasicToken ('='  : '=' : next) = Just (TokEq,      next)
parseBasicToken ('+'  : '=' : next) = Just (TokAddAsgn, next)
parseBasicToken ('*'  : '=' : next) = Just (TokMulAsgn, next)
parseBasicToken ('-'  : '=' : next) = Just (TokSubAsgn, next)
parseBasicToken ('/'  : '=' : next) = Just (TokDivAsgn, next)
parseBasicToken ('+'  : next) = Just (TokAdd,  next)
parseBasicToken ('*'  : next) = Just (TokMul,  next)
parseBasicToken ('-'  : next) = Just (TokSub,  next)
parseBasicToken ('/'  : next) = Just (TokDiv,  next)
parseBasicToken ('!'  : next) = Just (TokNot,  next)
parseBasicToken (':'  : next) = Just (TokCol,  next)
parseBasicToken ('>'  : next) = Just (TokGt,   next)
parseBasicToken ('<'  : next) = Just (TokLt,   next)
parseBasicToken ('='  : next) = Just (TokAsgn, next)
parseBasicToken _ = Nothing


parseWordToken :: ParserLexer
parseWordToken ('c' : 'o' : 'n' : 't' : 'i' : 'n' : 'u' : 'e' : next) = Just (TokContinue, next) -- <continue>
parseWordToken ('f' : 'a' : 'l' : 's' : 'e' : next) = Just (TokLit (LitBool False), next) -- <false>
parseWordToken ('s' : 't' : 'r' : 'i' : 'n' : 'g' : next) = Just (TokTyString, next) -- <string>
parseWordToken ('s' : 't' : 'r' : 'u' : 'c' : 't' : next) = Just (TokStruct, next) -- <struct>
parseWordToken ('r' : 'e' : 't' : 'u' : 'r' : 'n' : next) = Just (TokReturn, next) -- <return>
parseWordToken ('t' : 'r' : 'u' : 'e' : next) = Just (TokLit (LitBool True), next) -- <true>
parseWordToken ('f' : 'l' : 'o' : 'a' : 't' : next) = Just (TokTyFloat, next) -- <float>
parseWordToken ('w' : 'h' : 'i' : 'l' : 'e' : next) = Just (TokWhile, next) -- <while>
parseWordToken ('b' : 'r' : 'e' : 'a' : 'k' : next) = Just (TokBreak, next) -- <break>
parseWordToken ('c' : 'o' : 'n' : 's' : 't' : next) = Just (TokConst, next) -- <const>
parseWordToken ('t' : 'y' : 'p' : 'e' : next) = Just (TokDeclType, next) -- <type>
parseWordToken ('c' : 'h' : 'a' : 'r' : next) = Just (TokTyChar, next) -- <char>
parseWordToken ('b' : 'o' : 'o' : 'l' : next) = Just (TokTyBool, next) -- <bool>
parseWordToken ('v' : 'o' : 'i' : 'd' : next) = Just (TokTyVoid, next) -- <void>
parseWordToken ('e' : 'n' : 'u' : 'm' : next) = Just (TokEnum, next) -- <enum>
parseWordToken ('l' : 'o' : 'o' : 'p' : next) = Just (TokLoop, next) -- <loop>
parseWordToken ('e' : 'l' : 's' : 'e' : next) = Just (TokElse, next) -- <else>
parseWordToken ('u' : 's' : 'e' : next) = Just (TokImport, next) -- <use>
parseWordToken ('i' : 'n' : 't' : next) = Just (TokTyInt, next) -- <int>
parseWordToken ('f' : 'u' : 'n' : next) = Just (TokFun, next) -- <fun>
parseWordToken ('f' : 'o' : 'r' : next) = Just (TokFor, next) -- <for>
parseWordToken ('l' : 'e' : 't' : next) = Just (TokVar, next) -- <let>
parseWordToken ('n' : 'o' : 't' : next) = Just (TokNot, next) -- <not>
parseWordToken ('a' : 'n' : 'd' : next) = Just (TokAnd, next) -- <and>
parseWordToken ('a' : 's' : next) = Just (TokRName, next) -- <as>
parseWordToken ('i' : 'n' : next) = Just (TokIn, next) -- <in>
parseWordToken ('i' : 'f' : next) = Just (TokIf, next) -- <if>
parseWordToken ('o' : 'r' : next) = Just (TokOr, next) -- <or>
parseWordToken _ = Nothing


parseLitToken :: ParserLexer
parseLitToken = parseLitToken' []
  where
    parseLitToken' :: String -> String -> Maybe (Token, String)
    parseLitToken' []  [] = Nothing
    parseLitToken' acc []
      | isNumber acc      = Just (TokLit (LitInt   (read acc)), [])
      | isNumberOrDot acc = Just (TokLit (LitFloat (read acc)), [])
      | otherwise         = Nothing
    parseLitToken' [] ('\'' : '\\' : c : '\'' : next) = Just (TokLit (LitSChar c), next)
    parseLitToken' [] ('\'' :        c : '\'' : next) = Just (TokLit (LitChar  c), next)
    parseLitToken' [] ('"' : next) =
      case extractBtwQuot ('"' : next) of
        Just str -> Just (TokLit (LitString str), drop (length str + 1) next)
        Nothing  -> Nothing
    parseLitToken' [] (c : next)
      | isNumberOrDot [c] = parseLitToken' [c] next
      | otherwise         = Nothing
    parseLitToken' acc (c : next)
      | isNumberOrDot [c] && isNumberOrDot acc = parseLitToken' (acc ++ [c]) next
      | isNumber acc      = Just (TokLit (LitInt   (read acc)), c : next)
      | isNumberOrDot acc = Just (TokLit (LitFloat (read acc)), c : next)
      | otherwise         = Nothing


parseIdeToken :: ParserLexer
parseIdeToken [] = Nothing
parseIdeToken (c : next)
  | isAlphaNum c = parseIdeToken' [] (c : next)
  | otherwise    = Nothing
  where
    parseIdeToken' :: String -> String -> Maybe (Token, String)
    parseIdeToken' []  [] = Nothing
    parseIdeToken' acc [] = Just (TokIde acc, [])
    parseIdeToken' acc ('_' : n) = parseIdeToken' (acc ++ ['_']) n
    parseIdeToken' acc ( cb : n)
      | isAlphaNum cb = parseIdeToken' (acc ++ [cb]) n
      | otherwise     = Just (TokIde acc, cb : n)
