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
      [ parsePrintStack
      , parseLiteral
      , parseOperation
      , parseVarBinding
      , parseIdCall
      , parsePrintFunction
      , parsePrintLnFunction
      , parseFunctionBinding
      , parseFunctionCall
      , parseIf
      , parseReBound
      ]


type BcParser = [Ast] -> Maybe (Insts, [Ast])


parseLiteral :: BcParser
parseLiteral (Value (LitString v) : next) = Just (Push ArrayStart : parseLiteral' v, next)
  where
    parseLiteral' :: [Char] -> Insts
    parseLiteral' [     ] = [Push ArrayEnd]
    parseLiteral' (c : n) = Push (Char c) : parseLiteral' n
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
parseOperation (Section [Operator op x y] : next) = parseOperation (Operator op x y : next)
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


parsePrintStack :: BcParser
parsePrintStack (Section [CallFun "stack" code] : next) =
  case build [PrintStack] code of
    Just newInsts -> Just (newInsts, next)
    Nothing       -> Nothing
  where
    build :: Insts -> [Ast] -> Maybe Insts
    build acc [  ] = Just acc
    build acc steps =
      case getBC steps of
        Just (inst, n) -> build (acc ++ addEverySteps inst) n
        _              -> Nothing
    addEverySteps :: Insts -> Insts
    addEverySteps [     ] = []
    addEverySteps (a : n) = [a, PrintStack] ++ addEverySteps n
parsePrintStack _ = Nothing


parsePrintLnFunction :: BcParser
parsePrintLnFunction (Section [CallFun "println" code] : next) =
  case getBC code of
    Just (i, []) -> Just (i ++ [PrintTop, Push $ Char '\n' , PrintTop], next)
    _            -> Nothing
parsePrintLnFunction _ = Nothing


parsePrintFunction :: BcParser
parsePrintFunction (Section [CallFun "print" code] : next) =
  case getBC code of
    Just (i, []) -> Just (i ++ [PrintTop], next)
    _            -> Nothing
parsePrintFunction _ = Nothing


parseVarBinding :: BcParser
parseVarBinding (Binding (Bound name _ value) : next) =
  case getBC [value] of
    Just (vv, []) -> Just ([SaveToEnv name vv], next)
    _             -> Nothing
parseVarBinding _ = Nothing


parseIdCall :: BcParser
parseIdCall (CallId name : next) = Just ([PushFromEnv name], next)
parseIdCall _ = Nothing


parseFunctionBinding :: BcParser
parseFunctionBinding (Binding (Function funName arguments _ body) : next) =
  case handleFunction [] (formatArgs 0 arguments) body of
    Nothing    -> Nothing
    Just bytes -> Just ([SaveToEnv funName bytes], next)
  where
    formatArgs :: Int -> [AstBinding] -> [(String, Int)]
    formatArgs _ [] = []
    formatArgs i (Arg name _ : n) = (name, i) : formatArgs (i + 1) n
    formatArgs i (_          : n) = formatArgs i n

    handleFunction :: Insts -> [(String, Int)] -> [Ast] -> Maybe Insts
    handleFunction insts _ []      = Just insts
    handleFunction insts args code =
      case getBC code of
        Nothing           -> Nothing
        Just (sec, other) -> handleFunction (insts ++ handleArgCall args sec) args other
    
    handleArgCall :: [(String, Int)] -> Insts -> Insts
    handleArgCall _ [] = []
    handleArgCall args (PushFromEnv maybeArg : n) =
      case isArgs args maybeArg of
        Nothing        -> PushFromEnv maybeArg : handleArgCall args n
        Just subtitute -> subtitute            : handleArgCall args n
    handleArgCall args (c : n) = c : handleArgCall args n
    
    isArgs :: [(String, Int)] -> String -> Maybe Instruction
    isArgs [] _ = Nothing
    isArgs ((argName, index) : n) ideName
      | argName == ideName = Just $ PushFromArg index
      | otherwise          = isArgs n ideName
parseFunctionBinding _ = Nothing


parseFunctionCall :: BcParser
parseFunctionCall (Section [CallFun funName arguments] : next) = parseFunctionCall (CallFun funName arguments : next)
parseFunctionCall (CallFun funName arguments : next) =
  case parseArgumentsList [] arguments of
    Nothing  -> Nothing
    Just arg -> Just (arg ++ [PushFromEnv funName], next)
  where
    parseArgumentsList :: Insts -> [Ast] -> Maybe Insts
    parseArgumentsList insts [        ] = Just insts
    parseArgumentsList insts (a : n) =
      case getBC [a] of
        Just (i, []) -> parseArgumentsList (insts ++ i) n
        _            -> Nothing
parseFunctionCall _ = Nothing


parseIf :: BcParser
parseIf (Control (If cond thenBlock [Empty]) : next) =
  case getBC [cond] of
    Just (icond, []) ->
      case getBC thenBlock of
        Just (tb, []) -> Just (icond ++ [JumpIfFalse $ length tb] ++ tb, next)
        _             -> Nothing
    _  -> Nothing
parseIf (Control (If cond thenBlock elseBlock) : next) =
  case getBC [cond] of
    Just (icond, []) ->
      case (getBC thenBlock, getBC elseBlock) of
        (Just (tb, []), Just (eb, [])) ->
          Just (icond ++ [JumpIfFalse $ length tb + 2] ++ tb ++ [Push $ Bool False, JumpIfFalse $ length eb] ++ eb, next)
        _ -> Nothing
    _  -> Nothing
parseIf _ = Nothing


parseReBound :: BcParser
parseReBound (Binding (ReBound varName value) : next) = 
  case getBC [value] of
    Just (vv, []) -> Just ([SaveToEnv varName vv], next)
    _             -> Nothing
parseReBound _ = Nothing
