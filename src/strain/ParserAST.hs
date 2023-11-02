module ParserAST (buildASTtree, Ast(..)) where


import BlockExpr (BExpr(..))
import Data (Literal(..))
import Lexer (Token(..))


data AstType
  = TyString
  | TyFloat
  | TyBool
  | TyVoid
  | TyChar
  | TyInt
  | TyCustom String
  | TyArray  AstType
  deriving (Show, Eq)

data AstBinding
  = Bound    String  AstType Ast
  | ReBound  String  Ast
  | Arg      String  AstType
  | BConst   String  AstType Ast
  | Type     String  AstType
  | Enum     String [AstBinding]
  | Struct   String [AstBinding]
  | Function String [AstBinding] AstType [Ast]
  deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Mod | Not | And | Or | Gt | Lt | Eq deriving (Show, Eq)

data Control
  = If     Ast [Ast] [Ast]
  | While  Ast  Ast
  | Return Ast
  | For    AstBinding Ast Ast Ast
  deriving (Show, Eq)

data Ast
  = Empty
  | Program  [Ast]
  | Module    String [Ast]
  | Section  [Ast]
  | Value     Literal
  | CallId    String
  | CallFun   String [Ast]
  | Binding   AstBinding
  | Operator  Operator Ast Ast
  | Control   Control
  deriving (Show, Eq)


buildASTtree :: BExpr -> Either String Ast
buildASTtree (BModule name es) =
  case getAst es of
    Left  err -> Left err
    Right ast -> Right $ Module name ast
buildASTtree e = Left $ "<expect module but got> #" ++ show e ++ "#"


getAst :: [BExpr] -> Either String [Ast]
getAst = parseAST []


singleAst :: BExpr -> Maybe Ast
singleAst expr =
  case getAst [expr] of
    Right [ast] -> Just ast
    _           -> Nothing


parseAST :: [Ast] -> [BExpr] -> Either String [Ast]
parseAST _ (BModule name _ : _) = Left $ "<cannot have module inside other module> #" ++ name ++ "#"
parseAST accu [] = Right accu
parseAST accu ess = tryToParse accu ess parsers
  where
    tryToParse :: [Ast] -> [BExpr] -> [AstParser] -> Either String [Ast]
    tryToParse acc es [] = Left $ "<unmanaged situation>\n  #\n    acc = {\n      " ++ show acc ++ "\n    }\n    exprs = {\n      " ++ show es ++ "\n    }\n  #"
    tryToParse acc es (p : ps) =
      case p acc es of
        Nothing -> tryToParse acc es ps
        Just (new_ast, next) -> parseAST new_ast next

    parsers :: [AstParser]
    parsers =
      [ parseLiteral
      , parseNumberOperation
      , parseBooleanOperation
      , parseCmpOperation
      , parseInPrth
      , parseArray
      , parseBound
      , parseFunctionCall
      , parseIncrAndBound
      , parseFunction
      , parseStruct
      , parseEnum
      , parseTypeDecl
      , parseIf
      , parseSection
      , parseReturn
      ]


type AstParser = [Ast] -> [BExpr] -> Maybe ([Ast], [BExpr])


parseLiteral :: AstParser
parseLiteral _   (T (TokLit   _) : T (TokLit _) : _) = Nothing
parseLiteral acc (T (TokLit lit) : next) = Just (acc ++ [Value lit], next)
parseLiteral _ _ = Nothing


parseSection :: AstParser
parseSection acc (BSection section : next) =
  case getAst section of
    Left  _ -> Nothing
    Right s -> Just (acc ++ [Section s], next)
parseSection _ _ = Nothing


parseNumberOperation :: AstParser
parseNumberOperation [] _ = Nothing
parseNumberOperation acc (T op : y : T mop : next)
  | isPriorOp op && isPriorOp mop =
      case parseNumberOperation acc [T op, y] of
        Just (new, []) -> parseNumberOperation new (T mop : next)
        _ -> Nothing
  | not (isPriorOp op) && isPriorOp mop =
      case singleAst y of
        Nothing -> Nothing
        Just yv ->
          case parseNumberOperation [yv] (T mop : next) of
            Just ([zv], new_next) ->
              case op of
                TokAdd -> Just (init acc ++ [Operator Add (last acc) zv], new_next)
                TokSub -> Just (init acc ++ [Operator Sub (last acc) zv], new_next)
                TokMul -> Just (init acc ++ [Operator Mul (last acc) zv], new_next)
                TokDiv -> Just (init acc ++ [Operator Div (last acc) zv], new_next)
                TokMod -> Just (init acc ++ [Operator Mod (last acc) zv], new_next)
                _      -> Nothing
            _ -> Nothing
  | otherwise =
      case parseNumberOperation acc [T op, y] of
        Just (new, []) -> Just (new, T mop : next)
        _ -> Nothing
  where
    isPriorOp :: Token -> Bool
    isPriorOp o = o `elem` [TokMul, TokDiv, TokMod]
parseNumberOperation acc (T op : y : next) =
  case singleAst y of
    Nothing -> Nothing
    Just yv ->
      case op of
        TokAdd -> Just (init acc ++ [Operator Add (last acc) yv], next)
        TokSub -> Just (init acc ++ [Operator Sub (last acc) yv], next)
        TokMul -> Just (init acc ++ [Operator Mul (last acc) yv], next)
        TokDiv -> Just (init acc ++ [Operator Div (last acc) yv], next)
        TokMod -> Just (init acc ++ [Operator Mod (last acc) yv], next)
        _      -> Nothing
parseNumberOperation _ _ = Nothing


parseBooleanOperation :: AstParser
parseBooleanOperation acc (T op : y : next) =
  case singleAst y of
    Nothing -> Nothing
    Just yv ->
      case op of
        TokAnd -> Just (init acc ++ [Operator And (last acc) yv], next)
        TokNot -> Just (acc      ++ [Operator Not  yv     Empty], next)
        TokOr  -> Just (init acc ++ [Operator Or  (last acc) yv], next)
        _      -> Nothing
parseBooleanOperation _ _ = Nothing


parseCmpOperation :: AstParser
parseCmpOperation acc (T op : y : next) =
  case singleAst y of
    Nothing -> Nothing
    Just yv ->
      case op of
        TokEq    -> Just (init acc ++ [Operator Eq (last acc) yv], next)
        TokNotEq -> Just (init acc ++ [Operator Not (Operator Eq (last acc) yv) Empty], next)
        TokGt    -> Just (init acc ++ [Operator Gt (last acc) yv], next)
        TokLt    -> Just (init acc ++ [Operator Lt (last acc) yv], next)
        TokGtEq  -> Just (init acc ++ [Operator Or (Operator Gt (last acc) yv) (Operator Eq (last acc) yv)], next)
        TokLtEq  -> Just (init acc ++ [Operator Or (Operator Lt (last acc) yv) (Operator Eq (last acc) yv)], next)
        _        -> Nothing
parseCmpOperation _ _ = Nothing


parseInPrth :: AstParser
parseInPrth acc (Prths block : next) =
  case getAst block of
    Left  _   -> Nothing
    Right ast -> Just (acc ++ [Section ast], next)
parseInPrth _ _ = Nothing


parseArray :: AstParser
parseArray acc (Hooks elems : next) =
  case parseArray' [] elems of
    Just arr -> Just (acc ++ [Value $ LitArray arr], next)
    Nothing  -> Nothing
  where
    parseArray' :: [Literal] -> [BExpr] -> Maybe [Literal]
    parseArray' a [] = Just a
    parseArray' a [T (TokLit lit)] = parseArray' (a ++ [lit]) []
    parseArray' a (T (TokLit lit) : T TokCom : n)  = parseArray' (a ++ [lit]) n
    parseArray' _ _ = Nothing
parseArray _ _ = Nothing


parseBound :: AstParser
parseBound acc (BSection maybeBinding : next) =
  case parseBound' maybeBinding of
    Nothing -> Nothing
    Just bi -> Just (acc ++ [bi], next)
  where
    parseBound' :: [BExpr] -> Maybe Ast
    parseBound' (T TokVar : T (TokIde name) : T TokCol : T mtype : T TokAsgn : value) =
      case getAst value of
        Right [vv] ->
          case mtype of
            TokTyString -> Just $ Binding $ Bound name TyString vv
            TokTyFloat  -> Just $ Binding $ Bound name TyFloat  vv
            TokTyChar   -> Just $ Binding $ Bound name TyChar   vv
            TokTyInt    -> Just $ Binding $ Bound name TyInt    vv
            TokTyBool   -> Just $ Binding $ Bound name TyBool   vv
            TokTyVoid   -> Just $ Binding $ Bound name TyVoid   vv
            _           -> Nothing
        _ -> Nothing
    parseBound' (T TokVar : T (TokIde name) : T TokCol : T mtype : Hooks [] : T TokAsgn : value) =
      case getAst value of
        Right [vv] ->
          case mtype of
            TokTyString -> Just $ Binding $ Bound name (TyArray TyString) vv
            TokTyFloat  -> Just $ Binding $ Bound name (TyArray TyFloat)  vv
            TokTyChar   -> Just $ Binding $ Bound name (TyArray TyChar)   vv
            TokTyInt    -> Just $ Binding $ Bound name (TyArray TyInt)    vv
            TokTyBool   -> Just $ Binding $ Bound name (TyArray TyBool)   vv
            TokTyVoid   -> Just $ Binding $ Bound name (TyArray TyVoid)   vv
            _           -> Nothing
        _ -> Nothing
    parseBound' (T TokConst : T (TokIde name) : T TokCol : T mtype : T TokAsgn : value) =
      case getAst value of
        Right [vv] ->
          case mtype of
            TokTyString -> Just $ Binding $ BConst name TyString vv
            TokTyFloat  -> Just $ Binding $ BConst name TyFloat  vv
            TokTyChar   -> Just $ Binding $ BConst name TyChar   vv
            TokTyInt    -> Just $ Binding $ BConst name TyInt    vv
            TokTyBool   -> Just $ Binding $ BConst name TyBool   vv
            TokTyVoid   -> Just $ Binding $ BConst name TyVoid   vv
            _           -> Nothing
        _ -> Nothing
    parseBound' (T TokConst : T (TokIde name) : T TokCol : T mtype : Hooks [] : T TokAsgn : value) =
      case getAst value of
        Right [vv] ->
          case mtype of
            TokTyString -> Just $ Binding $ BConst name (TyArray TyString) vv
            TokTyFloat  -> Just $ Binding $ BConst name (TyArray TyFloat)  vv
            TokTyChar   -> Just $ Binding $ BConst name (TyArray TyChar)   vv
            TokTyInt    -> Just $ Binding $ BConst name (TyArray TyInt)    vv
            TokTyBool   -> Just $ Binding $ BConst name (TyArray TyBool)   vv
            TokTyVoid   -> Just $ Binding $ BConst name (TyArray TyVoid)   vv
            _           -> Nothing
        _ -> Nothing
    parseBound' _ = Nothing
parseBound _ _ = Nothing


parseIncrAndBound :: AstParser
parseIncrAndBound acc (BSection maybeIncrBinding : next) =
  case parseIncrAndBound' maybeIncrBinding of
    Nothing -> Nothing
    Just bi -> Just (acc ++ [bi], next)
  where
    parseIncrAndBound' :: [BExpr] -> Maybe Ast
    parseIncrAndBound' (T (TokIde name) : T op : value) =
      case getAst value of
        Right [vv] ->
          case op of
            TokAddAsgn -> Just $ Binding $ ReBound name $ Operator Add (CallId name) vv
            TokMulAsgn -> Just $ Binding $ ReBound name $ Operator Mul (CallId name) vv
            TokSubAsgn -> Just $ Binding $ ReBound name $ Operator Sub (CallId name) vv
            TokDivAsgn -> Just $ Binding $ ReBound name $ Operator Div (CallId name) vv
            _ -> Nothing
        _ -> Nothing
    parseIncrAndBound' _ = Nothing
parseIncrAndBound _ _ = Nothing


parseFunction :: AstParser
parseFunction acc (T TokFun : T (TokIde funName) : Prths args : T TokRetTy : T tokType : Braces body : next) =
  case parseArgs [] args of
    Nothing -> Nothing
    Just va ->
      case getAst body of
        Left  _ -> Nothing
        Right b ->
          case tokType of
            TokTyString -> Just (acc ++ [Binding $ Function funName va TyString b], next)
            TokTyFloat  -> Just (acc ++ [Binding $ Function funName va TyFloat  b], next)
            TokTyChar   -> Just (acc ++ [Binding $ Function funName va TyChar   b], next)
            TokTyInt    -> Just (acc ++ [Binding $ Function funName va TyInt    b], next)
            TokTyBool   -> Just (acc ++ [Binding $ Function funName va TyBool   b], next)
            TokTyVoid   -> Just (acc ++ [Binding $ Function funName va TyVoid   b], next)
            _           -> Nothing
  where
    parseArgs :: [AstBinding] -> [BExpr] -> Maybe [AstBinding]
    parseArgs [] [] = Just []
    parseArgs a [T (TokIde argName), T TokCol, T tokT] =
      case tokT of
        TokTyString -> Just (a ++ [Arg argName TyString])
        TokTyFloat  -> Just (a ++ [Arg argName TyString])
        TokTyChar   -> Just (a ++ [Arg argName TyString])
        TokTyInt    -> Just (a ++ [Arg argName TyString])
        TokTyBool   -> Just (a ++ [Arg argName TyString])
        TokTyVoid   -> Just (a ++ [Arg argName TyString])
        _           -> Nothing
    parseArgs a (T (TokIde argName) : T TokCol : T tokT : T TokCom : n) =
      case tokT of
        TokTyString -> parseArgs (a ++ [Arg argName TyString]) n
        TokTyFloat  -> parseArgs (a ++ [Arg argName TyString]) n
        TokTyChar   -> parseArgs (a ++ [Arg argName TyString]) n
        TokTyInt    -> parseArgs (a ++ [Arg argName TyString]) n
        TokTyBool   -> parseArgs (a ++ [Arg argName TyString]) n
        TokTyVoid   -> parseArgs (a ++ [Arg argName TyString]) n
        _           -> Nothing
    parseArgs _ _ = Nothing
parseFunction _ _ = Nothing


parseStruct :: AstParser
parseStruct acc (T TokStruct : T (TokIde structName) : Braces e : next) =
  case parseStruct' [] e of
    Nothing -> Nothing
    Just ss -> Just (acc ++ [Binding $ Struct structName ss], next)
  where
    parseStruct' :: [AstBinding] -> [BExpr] -> Maybe [AstBinding]
    parseStruct' a [] = Just a
    parseStruct' a [T (TokIde varName), T TokCol, T tokType] =
      case tokType of
        TokTyString -> parseStruct' (a ++ [Arg varName TyString]) []
        TokTyFloat  -> parseStruct' (a ++ [Arg varName  TyFloat]) []
        TokTyChar   -> parseStruct' (a ++ [Arg varName   TyChar]) []
        TokTyInt    -> parseStruct' (a ++ [Arg varName    TyInt]) []
        TokTyBool   -> parseStruct' (a ++ [Arg varName   TyBool]) []
        TokTyVoid   -> parseStruct' (a ++ [Arg varName   TyVoid]) []
        _           -> Nothing
    parseStruct' a (T (TokIde varName) : T TokCol : T tokType : T TokCom : n) =
      case tokType of
        TokTyString -> parseStruct' (a ++ [Arg varName TyString]) n
        TokTyFloat  -> parseStruct' (a ++ [Arg varName  TyFloat]) n
        TokTyChar   -> parseStruct' (a ++ [Arg varName   TyChar]) n
        TokTyInt    -> parseStruct' (a ++ [Arg varName    TyInt]) n
        TokTyBool   -> parseStruct' (a ++ [Arg varName   TyBool]) n
        TokTyVoid   -> parseStruct' (a ++ [Arg varName   TyVoid]) n
        _           -> Nothing
    parseStruct' _ _ = Nothing
parseStruct _ _ = Nothing


parseEnum :: AstParser
parseEnum acc (T TokEnum : T (TokIde enumName) : Braces e : next) =
  case parseEnum' [] 0 e of
    Nothing -> Nothing
    Just ss -> Just (acc ++ [Binding $ Enum enumName ss], next)
  where
    parseEnum' :: [AstBinding] -> Int -> [BExpr] -> Maybe [AstBinding]
    parseEnum' a _ [] = Just a
    parseEnum' a i (T (TokIde argName) : T TokCom : n) =
      parseEnum' (a ++ [Bound argName TyInt (Value (LitInt i))]) (i + 1) n
    parseEnum' a i [T (TokIde argName)] =
      parseEnum' (a ++ [Bound argName TyInt (Value (LitInt i))]) (i + 1) []
    parseEnum' _ _ _ = Nothing
parseEnum _ _ = Nothing


parseTypeDecl :: AstParser
parseTypeDecl acc (BSection decltype : next) = 
  case parseTypeDecl' decltype of
    Nothing -> Nothing
    Just dt -> Just (acc ++ [dt], next)
  where
    parseTypeDecl' :: [BExpr] -> Maybe Ast
    parseTypeDecl' [T TokDeclType, T (TokIde typeName), T TokAsgn, T typeType, Hooks []] =
      case typeType of
        TokTyString -> Just $ Binding $ Type typeName (TyArray TyString)
        TokTyFloat  -> Just $ Binding $ Type typeName (TyArray  TyFloat)
        TokTyChar   -> Just $ Binding $ Type typeName (TyArray   TyChar)
        TokTyInt    -> Just $ Binding $ Type typeName (TyArray    TyInt)
        TokTyBool   -> Just $ Binding $ Type typeName (TyArray   TyBool)
        TokTyVoid   -> Just $ Binding $ Type typeName (TyArray   TyVoid)
        _           -> Nothing
    parseTypeDecl' [T TokDeclType, T (TokIde typeName), T TokAsgn, T typeType] =
      case typeType of
        TokTyString -> Just $ Binding $ Type typeName TyString
        TokTyFloat  -> Just $ Binding $ Type typeName TyFloat
        TokTyChar   -> Just $ Binding $ Type typeName TyChar
        TokTyInt    -> Just $ Binding $ Type typeName TyInt
        TokTyBool   -> Just $ Binding $ Type typeName TyBool
        TokTyVoid   -> Just $ Binding $ Type typeName TyVoid
        _           -> Nothing
    parseTypeDecl' _ = Nothing
parseTypeDecl _ _ = Nothing
 

parseIf :: AstParser
parseIf acc (T TokIf : Prths cond : Braces ifBody : T TokElse : Braces elseBody : next) =
  case getAst cond of
    Right [c] ->
      case getAst ifBody of
        Left  _ -> Nothing
        Right i ->
          case getAst elseBody of
            Left  _ -> Nothing
            Right e -> Just (acc ++ [Control $ If c i e], next)
    _ -> Nothing
parseIf acc (T TokIf : Prths cond : Braces ifBody : next) =
  case getAst cond of
    Right [c] ->
      case getAst ifBody of
        Left  _ -> Nothing
        Right i -> Just (acc ++ [Control $ If c i [Empty]], next)
    _ -> Nothing
parseIf _ _ = Nothing


parseReturn :: AstParser
parseReturn acc (BSection ret : next) =
  case parseReturn' ret of
    Nothing -> Nothing
    Just re -> Just (acc ++ [re], next)
  where
    parseReturn' :: [BExpr] -> Maybe Ast
    parseReturn' (T TokReturn : retValue) =
      case getAst retValue of
        Right [r] -> Just $ Control $ Return r
        _         -> Nothing
    parseReturn' _ = Nothing
parseReturn _ _ = Nothing


parseFunctionCall :: AstParser
parseFunctionCall acc (T (TokIde funName) : Prths args : next) =
  case parseArgs [] args of
    Nothing -> Nothing
    Just ag -> Just (acc ++ [CallFun funName ag], next)
  where
    parseArgs :: [Ast] -> [BExpr] -> Maybe [Ast]
    parseArgs [] [] = Just []
    parseArgs a  [] = Just a
    parseArgs a [value] =
      case singleAst value of
        Just e -> parseArgs (a ++ [e]) []
        _            -> Nothing
    parseArgs a (value : T TokCom : n) =
      case singleAst value of
        Just e -> parseArgs (a ++ [e]) n
        _            -> Nothing
    parseArgs _ _ = Nothing
parseFunctionCall _ _ = Nothing
