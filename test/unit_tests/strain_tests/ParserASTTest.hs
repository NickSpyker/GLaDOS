module ParserASTTest (testParse, 
                    testParseNumberOperation,
                    testParseAST, 
                    testParseBooleanOperation,
                    testParseCmpOperation,
                    testParseInPrth,
                    testParseArray,
                    testParseBound,
                    testParseIncrAndBound,
                    testParseStruct,
                    testParseEnum,
                    testParseTypeDecl,
                    testParseIf,
                    testParseReturn,
                    testParseFunctionCall,
                    testParseFunction
                    ) where
import Lexer (Token (..))
import Test.HUnit
import BlockExpr(BExpr(..))
import ParserAST(buildASTtree, 
                parseNumberOperation, 
                parseBooleanOperation,
                parseAST, 
                parseCmpOperation, 
                parseInPrth,
                parseArray,
                parseBound,
                parseIncrAndBound,
                parseFunction,
                parseStruct,
                parseEnum,
                parseTypeDecl,
                parseIf,
                parseReturn,
                parseFunctionCall,
                Ast(..), Control(..), Operator(..), AstType(..), AstBinding(..))
import Data (Literal(..))


-- Helper function to wrap Tokens.
t :: Token -> BExpr
t = T

-- Helper function to wrap literals.
l :: Literal -> BExpr
l lit = t (TokLit lit)

testParse :: Test
testParse =
  TestList 
    [
      "Parsing literal"
        ~: buildASTtree (BModule "testmodule" [T (TokLit (LitString "Hello"))])
        ~?= Right (Module "testmodule" [Value (LitString "Hello")]),
      
      "Parsing section"
        ~: buildASTtree (BModule "testmodule" [BSection [T (TokLit (LitString "Hello"))]])
        ~?= Right (Module "testmodule" [Section [Value (LitString "Hello")]]),
      
      "Parsing number operation"
        ~: buildASTtree (BModule "testmodule" [T (TokLit (LitInt 5)), T TokAdd, T (TokLit (LitInt 3))])
        ~?= Right (Module "testmodule" [Operator Add (Value (LitInt 5)) (Value (LitInt 3))]),
      
      "Parsing boolean operation"
        ~: buildASTtree (BModule "testmodule" [T (TokLit (LitBool True)), T TokAnd, T (TokLit (LitBool False))])
        ~?= Right (Module "testmodule" [Operator And (Value (LitBool True)) (Value (LitBool False))]),
      
      "Parsing comparison operation"
        ~: buildASTtree (BModule "testmodule" [T (TokLit (LitInt 5)), T TokEq, T (TokLit (LitInt 3))])
        ~?= Right (Module "testmodule" [Operator Eq (Value (LitInt 5)) (Value (LitInt 3))]),

      "buildASTtree error on non-module expression"
        ~: case buildASTtree (BSection [T (TokLit (LitString "Invalid"))]) of
          Left errMsg -> errMsg ~?= "<expect module but got> #BSection [T (TokLit (LitString \"Invalid\"))]#"
          Right _ -> False ~?= True,

      "Non-module input error"
        ~: case buildASTtree (BSection []) of
            Left err -> err ~?= "<expect module but got> #BSection []#"
            Right _ -> "Expected error for non-module input, got success" ~?= "Expected failure"
    ]

testParseAST :: Test
testParseAST =
  TestList 
    [
      "parseAST - error for module inside module"
        ~: parseAST [] [BModule "inner" []]
        ~?= Left "<cannot have module inside other module> #inner#"

      -- "parseAST - error for unmanaged situation"
      --   ~: parseAST [] ["prouttt"]
      --   ~?= Left "<unmanaged situation>\n  #\n    acc = {\n      []\n    }\n    exprs = {\n      [your unhandled expression here]\n    }\n  #"

    ]

testParseNumberOperation :: Test
testParseNumberOperation = TestList [
    "Test single addition"
      ~: parseNumberOperation [Value (LitInt 1)] [t TokAdd, l (LitInt 2)]
      ~?= Just  ([Operator Add (Value (LitInt 1)) (Value (LitInt 2))], []),
    
    "Test single subtraction"
      ~: parseNumberOperation [Value (LitInt 3)] [t TokSub, l (LitInt 2)]
      ~?= Just  ([Operator Sub (Value (LitInt 3)) (Value (LitInt 2))], []),
    
    "Test multiplication then addition"
      ~: parseNumberOperation [Value (LitInt 3)] [t TokAdd, l (LitInt 4), t TokMul, l (LitInt 5)]
      ~?= Just  ([Operator Add (Value (LitInt 3)) (Operator Mul (Value (LitInt 4)) (Value (LitInt 5)))], []),
    
    "Test multiplication then subtraction"
      ~: parseNumberOperation [Value (LitInt 3)] [t TokSub, l (LitInt 4), t TokMul, l (LitInt 5)]
      ~?= Just  ([Operator Sub (Value (LitInt 3)) (Operator Mul (Value (LitInt 4)) (Value (LitInt 5)))], []),

    "Test multiple multiplications"
      ~: parseNumberOperation [Value (LitInt 3)] [t TokMul, l (LitInt 4), t TokMul, l (LitInt 5)]
      ~?= Just  ([Operator Mul (Operator Mul (Value (LitInt 3)) (Value (LitInt 4))) (Value (LitInt 5))], []),
    
    "Test missing operand"
      ~: parseNumberOperation [Value (LitInt 1)] [t TokAdd]
      ~?= Nothing,
    
    "Test missing operation"
      ~: parseNumberOperation [Value (LitInt 1), Value (LitInt 2)] []
      ~?= Nothing,
    
    "Test division then subtraction"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokSub, l (LitInt 3), t TokDiv, l (LitInt 2)]
      ~?= Just ([Operator Sub (Value (LitInt 6)) (Operator Div (Value (LitInt 3)) (Value (LitInt 2)))], []),
    
    "Test single division"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokDiv, l (LitInt 2)]
      ~?= Just ([Operator Div (Value (LitInt 6)) (Value (LitInt 2))], []),
    
    "Test single modulo"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokMod, l (LitInt 4)]
      ~?= Just ([Operator Mod (Value (LitInt 6)) (Value (LitInt 4))], []),

    "Test addition then multiplication"
      ~: parseNumberOperation [Value (LitInt 2)] [t TokAdd, l (LitInt 3), t TokMul, l (LitInt 4)]
      ~?= Just ([Operator Add (Value (LitInt 2)) (Operator Mul (Value (LitInt 3)) (Value (LitInt 4)))], []),

    "Test subtraction then division"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokSub, l (LitInt 4), t TokDiv, l (LitInt 2)]
      ~?= Just ([Operator Sub (Value (LitInt 6)) (Operator Div (Value (LitInt 4)) (Value (LitInt 2)))], []),

    "Test single addition"
      ~: parseNumberOperation [Value (LitInt 1)] [t TokAdd, l (LitInt 2)]
      ~?= Just ([Operator Add (Value (LitInt 1)) (Value (LitInt 2))], []),

    "Test single division"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokDiv, l (LitInt 3)]
      ~?= Just ([Operator Div (Value (LitInt 6)) (Value (LitInt 3))], []),

    "Test single modulo"
      ~: parseNumberOperation [Value (LitInt 7)] [t TokMod, l (LitInt 3)]
      ~?= Just ([Operator Mod (Value (LitInt 7)) (Value (LitInt 3))], []),

    "Test invalid token sequence"
      ~: parseNumberOperation [Value (LitInt 1)] [t TokAdd, t TokAdd]
      ~?= Nothing,

    "Test single operator without operand"
      ~: parseNumberOperation [Value (LitInt 1)] [t TokMul]
      ~?= Nothing,

    "Test only operands without operator"
      ~: parseNumberOperation [Value (LitInt 1), Value (LitInt 2)] []
      ~?= Nothing,

    "Test multiplication then multiplication"
      ~: parseNumberOperation [Value (LitInt 2)] [t TokMul, l (LitInt 3), t TokMul, l (LitInt 4)]
      ~?= Just ([Operator Mul (Operator Mul (Value (LitInt 2)) (Value (LitInt 3))) (Value (LitInt 4))],[]),
    "Test multiplication then division"
      ~: parseNumberOperation [Value (LitInt 6)] [t TokMul, l (LitInt 2), t TokDiv, l (LitInt 2)]
      ~?= Just ([Operator Div (Operator Mul (Value (LitInt 6)) (Value (LitInt 2))) (Value (LitInt 2))],[]),

    "Negative integer parsing"
      ~: parseNumberOperation [] [T TokSub, T (TokLit (LitInt 10))]
      ~?= Just ([Value (LitInt (-10))], []),

    "Negative float parsing"
      ~: parseNumberOperation [] [T TokSub, T (TokLit (LitFloat 10.5))]
      ~?= Just ([Value (LitFloat (-10.5))], []),

    "Multiplication with previous accumulator"
      ~: parseNumberOperation [Value (LitInt 6)] [T TokMul, T (TokLit (LitInt 4))]
      ~?= Just ([Operator Mul (Value (LitInt 6)) (Value (LitInt 4))], []),

    "Division parsed correctly"
      ~: parseNumberOperation [Value (LitInt 12)] [T TokDiv, T (TokLit (LitInt 4))]
      ~?= Just ([Operator Div (Value (LitInt 12)) (Value (LitInt 4))], [])
    ]

testParseBooleanOperation :: Test
testParseBooleanOperation = 
  TestList [
    "Testing parseBooleanOperation with AND operation"
    ~: parseBooleanOperation [Value (LitBool True)] [T TokAnd, T (TokLit (LitBool False))]
    ~?= Just ([Operator And (Value (LitBool True)) (Value (LitBool False))], []),

    "Testing parseBooleanOperation with NOT operation"
    ~: parseBooleanOperation [] [T TokNot, T (TokLit (LitBool False))]
    ~?= Just ([Operator Not (Value (LitBool False)) Empty], []),

    "Testing parseBooleanOperation with OR operation"
    ~: parseBooleanOperation [Value (LitBool False)] [T TokOr, T (TokLit (LitBool True))]
    ~?= Just ([Operator Or (Value (LitBool False)) (Value (LitBool True))], [])
  ]

testParseCmpOperation :: Test
testParseCmpOperation = TestList [
    "Testing parseCmpOperation with equality operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokEq, T (TokLit (LitInt 5))]
    ~?= Just ([Operator Eq (Value (LitInt 5)) (Value (LitInt 5))], []),

    "Testing parseCmpOperation with not equal operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokNotEq, T (TokLit (LitInt 3))]
    ~?= Just ([Operator Not (Operator Eq (Value (LitInt 5)) (Value (LitInt 3))) Empty], []),

    "Testing parseCmpOperation with greater than operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokGt, T (TokLit (LitInt 3))]
    ~?= Just ([Operator Gt (Value (LitInt 5)) (Value (LitInt 3))], []),

    "Testing parseCmpOperation with less than operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokLt, T (TokLit (LitInt 7))]
    ~?= Just ([Operator Lt (Value (LitInt 5)) (Value (LitInt 7))], []),

    "Testing parseCmpOperation with greater than or equal operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokGtEq, T (TokLit (LitInt 5))]
    ~?= Just ([Operator Or (Operator Gt (Value (LitInt 5)) (Value (LitInt 5))) (Operator Eq (Value (LitInt 5)) (Value (LitInt 5)))], []),

    "Testing parseCmpOperation with less than or equal operation"
    ~: parseCmpOperation [Value (LitInt 5)] [T TokLtEq, T (TokLit (LitInt 5))]
    ~?= Just ([Operator Or (Operator Lt (Value (LitInt 5)) (Value (LitInt 5))) (Operator Eq (Value (LitInt 5)) (Value (LitInt 5)))], [])
  ]

testParseInPrth :: Test
testParseInPrth = TestList [
    "Testing parseInPrth with a non-empty block"
    ~: parseInPrth [] [Prths [T (TokLit (LitInt 10))]]
    ~?= Just ([Section [Value (LitInt 10)]], []),

    "Testing parseInPrth with an empty block"
    ~: parseInPrth [] [Prths []]
    ~?= Just ([Section []], []),

    "Testing parseInPrth with failure case"
    ~: parseInPrth [] [Prths [T TokCom]]
    ~?= Nothing
  ]

testParseArray :: Test
testParseArray = TestList [
    "Testing parseArray with single element"
    ~: parseArray [] [Hooks [T (TokLit (LitInt 5))]]
    ~?= Just ([Value (LitArray [LitInt 5])], []),

    "Testing parseArray with multiple elements"
    ~: parseArray [] [Hooks [T (TokLit (LitInt 5)), T TokCom, T (TokLit (LitInt 10))]]
    ~?= Just ([Value (LitArray [LitInt 5, LitInt 10])], []),

    "Testing parseArray with nested empty arrays"
    ~: parseArray [] [Hooks [T (TokLit (LitArray [])), T TokCom, T (TokLit (LitArray []))]]
    ~?= Just ([Value (LitArray [LitArray [], LitArray []])], [])
  ]

testParseBound :: Test
testParseBound = 
  TestList [
    "Testing parseBound with variable binding of type Int"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "x"), T TokCol, T TokTyInt, T TokAsgn, T (TokLit (LitInt 10))]]
    ~?= Just ([Binding (Bound "x" TyInt (Value (LitInt 10)))], []),

    "Testing parseBound with constant binding of type Float"
    ~: parseBound [] [BSection [T TokConst, T (TokIde "pi"), T TokCol, T TokTyFloat, T TokAsgn, T (TokLit (LitFloat 3.1415))]]
    ~?= Just ([Binding (BConst "pi" TyFloat (Value (LitFloat 3.1415)))], []),

    "Testing parseBound with an unsupported type should fail"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "x"), T TokCol, T (TokLit (LitInt 5)), T TokAsgn, T (TokLit (LitInt 10))]]
    ~?= Nothing,

    "Test variable binding for String without hooks" 
    ~: parseBound [] [BSection [T TokVar, T (TokIde "str"), T TokCol, T TokTyString, T TokAsgn, T (TokLit (LitString "hello"))]]
    ~?= Just ([Binding (Bound "str" TyString (Value (LitString "hello")))], []),

    "Test variable binding for Int without hooks"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "intVar"), T TokCol, T TokTyInt, T TokAsgn, T (TokLit (LitInt 42))]]
    ~?= Just ([Binding (Bound "intVar" TyInt (Value (LitInt 42)))], []),

    "Test constant binding for String without hooks"
    ~: parseBound [] [BSection [T TokConst, T (TokIde "constStr"), T TokCol, T TokTyString, T TokAsgn, T (TokLit (LitString "constant"))]]
    ~?= Just ([Binding (BConst "constStr" TyString (Value (LitString "constant")))], []),

    "Test malformed input that doesn't match expected pattern triggers 'Nothing'"
    ~: parseBound [] [BSection [T (TokIde "broken"), T TokCol, T TokTyString]]
    ~?= Nothing,

    "Test variable binding for Float without hooks" 
    ~: parseBound [] [BSection [T TokVar, T (TokIde "floatVar"), T TokCol, T TokTyFloat, T TokAsgn, T (TokLit (LitFloat 3.14))]]
    ~?= Just ([Binding (Bound "floatVar" TyFloat (Value (LitFloat 3.14)))], []),

    "Test variable binding for Char without hooks"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "charVar"), T TokCol, T TokTyChar, T TokAsgn, T (TokLit (LitChar 'c'))]]
    ~?= Just ([Binding (Bound "charVar" TyChar (Value (LitChar 'c')))], []),

    "Test variable binding for Bool without hooks"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "boolVar"), T TokCol, T TokTyBool, T TokAsgn, T (TokLit (LitBool True))]]
    ~?= Just ([Binding (Bound "boolVar" TyBool (Value (LitBool True)))], []),

    "Test constant binding for array of String with hooks fails due to invalid value"
    ~: parseBound [] [BSection [T TokConst, T (TokIde "invalidArrayConstString"), T TokCol, Hooks [], T TokTyString, T TokAsgn, T (TokLit (LitInt 0))]]
    ~?= Nothing,

    "Test constant binding for array of Char with hooks fails due to invalid pattern"
    ~: parseBound [] [BSection [T TokConst, T (TokIde "invalidPatternArrayChar"), T TokCol, T (TokLit (LitChar 'a')), Hooks [], T TokAsgn, T (TokLit (LitChar 'a'))]]
    ~?= Nothing,

    "Test binding for unsupported array type should fail"
    ~: parseBound [] [BSection [T TokVar, T (TokIde "unsupportedArray"), T TokCol, Hooks [], T (TokLit (LitString "unknown")), T TokAsgn, T (TokLit (LitString "value"))]]
    ~?= Nothing

    -- "Test constant binding for Void without hooks"
    -- ~: parseBound [] [BSection [T TokConst, T (TokIde "voidConst"), T TokCol, T TokTyVoid, T TokAsgn, T (TokLit (LitInt 0))]]
    -- ~?= Just ([Binding (BConst "voidConst" TyVoid (Value (LitInt 0)))], []),

    -- "Testing parseBound with variable binding of array type String"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "strArray"), T TokCol, Hooks [], T TokTyString, T TokAsgn, T (TokLit (LitString "hello"))]]
    -- ~?= Just ([Binding (Bound "strArray" (TyArray TyString) (Value (LitString "hello")))], []),

    -- "Testing parseBound with variable binding of array type Float"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "floatArray"), T TokCol, Hooks [], T TokTyFloat, T TokAsgn, T (TokLit (LitFloat 1.23))]]
    -- ~?= Just ([Binding (Bound "floatArray" (TyArray TyFloat) (Value (LitFloat 1.23)))], []),

    -- "Testing parseBound with variable binding of array type Char"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "charArray"), T TokCol, Hooks [], T TokTyChar, T TokAsgn, T (TokLit (LitChar 'a'))]]
    -- ~?= Just ([Binding (Bound "charArray" (TyArray TyChar) (Value (LitChar 'a')))], []),

    -- "Testing parseBound with variable binding of array type Int"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "intArray"), T TokCol, Hooks [], T TokTyInt, T TokAsgn, T (TokLit (LitInt 5))]]
    -- ~?= Just ([Binding (Bound "intArray" (TyArray TyInt) (Value (LitInt 5)))], []),

    -- "Testing parseBound with variable binding of array type Bool"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "boolArray"), T TokCol, Hooks [], T TokTyBool, T TokAsgn, T (TokLit (LitBool True))]]
    -- ~?= Just ([Binding (Bound "boolArray" (TyArray TyBool) (Value (LitBool True)))], []),

    -- "Testing parseBound with variable binding of array type Void"
    -- ~: parseBound [] [BSection [T TokVar, T (TokIde "voidArray"), T TokCol, Hooks [], T TokTyVoid, T TokAsgn, T (TokLit (LitInt 0))]] -- Void type doesn't have a literal, so using Int as placeholder.
    -- ~?= Just ([Binding (Bound "voidArray" (TyArray TyVoid) (Value (LitInt 0)) )], [])

    ]

testParseIncrAndBound :: Test
testParseIncrAndBound = 
  TestList [

    "Testing parseIncrAndBound with increment and re-binding of += operator"
    ~: parseIncrAndBound [] [BSection [T (TokIde "x"), T TokAddAsgn, T (TokLit (LitInt 5))]]
    ~?= Just ([Binding $ ReBound "x" $ Operator Add (CallId "x") (Value (LitInt 5))], []),

    "Testing parseIncrAndBound with increment and re-binding of *= operator"
    ~: parseIncrAndBound [] [BSection [T (TokIde "y"), T TokMulAsgn, T (TokLit (LitInt 3))]]
    ~?= Just ([Binding $ ReBound "y" $ Operator Mul (CallId "y") (Value (LitInt 3))], []),

    "Testing parseIncrAndBound with increment and re-binding of -= operator"
    ~: parseIncrAndBound [] [BSection [T (TokIde "z"), T TokSubAsgn, T (TokLit (LitInt 2))]]
    ~?= Just ([Binding $ ReBound "z" $ Operator Sub (CallId "z") (Value (LitInt 2))], []),

    "Testing parseIncrAndBound with increment and re-binding of /= operator"
    ~: parseIncrAndBound [] [BSection [T (TokIde "w"), T TokDivAsgn, T (TokLit (LitFloat 4.56))]]
    ~?= Just ([Binding $ ReBound "w" $ Operator Div (CallId "w") (Value (LitFloat 4.56))], [])
 
  ]

testParseFunction :: Test
testParseFunction =
  TestList [
    "Testing parseFunction with valid function with string return type"
    ~: parseFunction [] [T TokFun, T (TokIde "foo"), Prths [], T TokRetTy, T TokTyString, Braces []]
    ~?= Just ([Binding $ Function "foo" [] TyString []], []),

    -- "Testing parseFunction with valid function with int return type and body"
    -- ~: parseFunction [] [T TokFun, T (TokIde "bar"), Prths [], T TokRetTy, T TokTyInt, Braces [T (TokLit (LitInt 42))]]
    -- ~?= Just ([Binding $ Function "bar" [] TyInt [Literal (LitInt 42)]], []),

    "Testing parseFunction with no arguments and float return type"
    ~: parseFunction [] [T TokFun, T (TokIde "baz"), Prths [], T TokRetTy, T TokTyFloat, Braces []]
    ~?= Just ([Binding $ Function "baz" [] TyFloat []], []),

    "Testing parseFunction with invalid token sequence"
    ~: parseFunction [] [T TokFun, T (TokIde "invalid"), Prths [], T TokTyInt, Braces []]
    ~?= Nothing,

    "Testing parseFunction with incomplete token sequence"
    ~: parseFunction [] [T TokFun, T (TokIde "incomplete"), Prths []]
    ~?= Nothing,

    "Testing parseFunction with void return type and single int array arg"
    ~: parseFunction [] [T TokFun, T (TokIde "withArg"), Prths [T (TokIde "arg1"), T TokCol, T TokTyInt, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding $ Function "withArg" [Arg "arg1" (TyArray TyInt)] TyVoid []], []),

    "Testing parseFunction with invalid argument type"
    ~: parseFunction [] [T TokFun, T (TokIde "wrongArg"), Prths [T (TokIde "arg1"), T TokCol, T (TokIde "UnknownType")], T TokRetTy, T TokTyString, Braces []]
    ~?= Nothing,

    "Testing parseFunction with valid function with char return type and body" ~: 
    parseFunction [] [T TokFun, T (TokIde "charFun"), Prths [], T TokRetTy, T TokTyChar, Braces [T (TokLit (LitChar 'a'))]] 
    ~?= Just ([Binding $ Function "charFun" [] TyChar [Value (LitChar 'a')]], []),

    "Testing parseFunction with valid function with int return type and body" ~: 
    parseFunction [] [T TokFun, T (TokIde "intFun"), Prths [], T TokRetTy, T TokTyInt, Braces [T (TokLit (LitInt 42))]] 
    ~?= Just ([Binding $ Function "intFun" [] TyInt [Value (LitInt 42)]], []),

    "Testing parseFunction with valid function with bool return type and body" ~: 
    parseFunction [] [T TokFun, T (TokIde "boolFun"), Prths [], T TokRetTy, T TokTyBool, Braces [T (TokLit (LitBool True))]] 
    ~?= Just ([Binding $ Function "boolFun" [] TyBool [Value (LitBool True)]], []),

    "Testing parseFunction with valid function with string array argument" ~:
    parseFunction [] [T TokFun, T (TokIde "stringArrayFun"), Prths [T (TokIde "arg1"), T TokCol, T TokTyString, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding $ Function "stringArrayFun" [Arg "arg1" (TyArray TyString)] TyVoid []], []),

    "Testing parseFunction with valid function with float array argument" ~:
    parseFunction [] [T TokFun, T (TokIde "floatArrayFun"), Prths [T (TokIde "arg1"), T TokCol, T TokTyFloat, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding $ Function "floatArrayFun" [Arg "arg1" (TyArray TyFloat)] TyVoid []], []),

    "Testing parseFunction with valid function with char array argument" ~:
    parseFunction [] [T TokFun, T (TokIde "charArrayFun"), Prths [T (TokIde "arg1"), T TokCol, T TokTyChar, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding $ Function "charArrayFun" [Arg "arg1" (TyArray TyChar)] TyVoid []], []),

    "Testing parseFunction with valid function with bool array argument" ~:
    parseFunction [] [T TokFun, T (TokIde "boolArrayFun"), Prths [T (TokIde "arg1"), T TokCol, T TokTyBool, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding $ Function "boolArrayFun" [Arg "arg1" (TyArray TyBool)] TyVoid []], []),

    "Testing parseFunction with invalid function with void array argument" ~:
    parseFunction [] [T TokFun, T (TokIde "voidArrayFun"), Prths [T (TokIde "arg1"), T TokCol, T TokTyVoid, Hooks []], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Just ([Binding (Function "voidArrayFun" [Arg "arg1" (TyArray TyVoid)] TyVoid [])],[]),

    "Testing parseFunction with wrong sequence after array type argument" 
    ~: parseFunction [] [T TokFun, T (TokIde "brokenFun"), Prths [T (TokIde "arg"), T TokCol, T TokTyInt, Hooks [], T TokCom], T TokRetTy, T TokTyVoid, Braces []]
    ~?= Nothing
  ]

testParseStruct :: Test
testParseStruct = TestList [
    "Testing parseStruct with no fields"
    ~: parseStruct [] [T TokStruct, T (TokIde "EmptyStruct"), Braces [], T TokSmCol]
    ~?= Just ([Binding $ Struct "EmptyStruct" []], [T TokSmCol]),

    "Testing parseStruct with one field"
    ~: parseStruct [] [T TokStruct, T (TokIde "SingleFieldStruct"), Braces [T (TokIde "field"), T TokCol, T TokTyInt], T TokSmCol]
    ~?= Just ([Binding $ Struct "SingleFieldStruct" [Arg "field" TyInt]], [T TokSmCol]),

    "Testing parseStruct with multiple fields"
    ~: parseStruct [] [T TokStruct, T (TokIde "TwoFieldStruct"), Braces [T (TokIde "field1"), T TokCol, T TokTyInt, T TokCom, T (TokIde "field2"), T TokCol, T TokTyString], T TokSmCol]
    ~?= Just ([Binding $ Struct "TwoFieldStruct" [Arg "field1" TyInt, Arg "field2" TyString]], [T TokSmCol]),

    "Testing parseStruct with incorrect pattern - missing type"
    ~: parseStruct [] [T TokStruct, T (TokIde "BadStruct"), Braces [T (TokIde "field"), T TokCol], T TokSmCol]
    ~?= Nothing,

    "Testing parseStruct with incorrect pattern - trailing comma"
    ~: parseStruct [] [T TokStruct, T (TokIde "TrailingCommaStruct"), Braces [T (TokIde "field"), T TokCol, T TokTyInt, T TokCom], T TokSmCol]
    ~?= Just ([Binding (Struct "TrailingCommaStruct" [Arg "field" TyInt])],[T TokSmCol]),

    "Testing parseStruct with subsequent tokens"
    ~: parseStruct [] [T TokStruct, T (TokIde "SubsequentTokenStruct"), Braces [T (TokIde "field1"), T TokCol, T TokTyInt], T TokCom, T (TokIde "NextToken")]
    ~?= Just ([Binding $ Struct "SubsequentTokenStruct" [Arg "field1" TyInt]], [T TokCom, T (TokIde "NextToken")]),

    -- Testing TyFloat field parsing in the middle of struct declaration
    "Testing parseStruct with TyFloat field"
    ~: parseStruct [] [T TokStruct, T (TokIde "FloatFieldStruct"), Braces [T (TokIde "floatField"), T TokCol, T TokTyFloat, T TokCom, T (TokIde "nextField"), T TokCol, T TokTyInt], T TokSmCol]
    ~?= Just ([Binding $ Struct "FloatFieldStruct" [Arg "floatField" TyFloat, Arg "nextField" TyInt]], [T TokSmCol]),

    -- Testing TyChar field parsing in the middle of struct declaration
    "Testing parseStruct with TyChar field"
    ~: parseStruct [] [T TokStruct, T (TokIde "CharFieldStruct"), Braces [T (TokIde "charField"), T TokCol, T TokTyChar, T TokCom, T (TokIde "nextField"), T TokCol, T TokTyString], T TokSmCol]
    ~?= Just ([Binding $ Struct "CharFieldStruct" [Arg "charField" TyChar, Arg "nextField" TyString]], [T TokSmCol]),

    -- Testing TyBool field parsing in the middle of struct declaration
    "Testing parseStruct with TyBool field"
    ~: parseStruct [] [T TokStruct, T (TokIde "BoolFieldStruct"), Braces [T (TokIde "boolField"), T TokCol, T TokTyBool, T TokCom, T (TokIde "nextField"), T TokCol, T TokTyFloat], T TokSmCol]
    ~?= Just ([Binding $ Struct "BoolFieldStruct" [Arg "boolField" TyBool, Arg "nextField" TyFloat]], [T TokSmCol]),

    -- Testing TyVoid field parsing in the middle of struct declaration
    "Testing parseStruct with TyVoid field"
    ~: parseStruct [] [T TokStruct, T (TokIde "VoidFieldStruct"), Braces [T (TokIde "voidField"), T TokCol, T TokTyVoid, T TokCom, T (TokIde "nextField"), T TokCol, T TokTyChar], T TokSmCol]
    ~?= Just ([Binding $ Struct "VoidFieldStruct" [Arg "voidField" TyVoid, Arg "nextField" TyChar]], [T TokSmCol]),

    -- Testing unrecognized token type within struct declaration
    "Testing parseStruct with unrecognized token type"
    ~: parseStruct [] [T TokStruct, T (TokIde "UnrecognizedTokenStruct"), Braces [T (TokIde "badField"), T TokCol, T (TokIde "unknownType")], T TokSmCol]
    ~?= Nothing,

    -- Testing TyFloat field parsing at the end of struct declaration with subsequent tokens
    "Testing parseStruct with TyFloat at the end with subsequent tokens"
    ~: parseStruct [] [T TokStruct, T (TokIde "FloatFieldEndStruct"), Braces [T (TokIde "floatField"), T TokCol, T TokTyFloat], T TokCom, T (TokIde "afterStruct")]
    ~?= Just ([Binding $ Struct "FloatFieldEndStruct" [Arg "floatField" TyFloat]], [T TokCom, T (TokIde "afterStruct")]),

    "Testing parseStruct with only a TyBool field"
    ~: parseStruct [] [T TokStruct, T (TokIde "BoolOnlyStruct"), Braces [T (TokIde "boolField"), T TokCol, T TokTyBool], T TokSmCol]
    ~?= Just ([Binding $ Struct "BoolOnlyStruct" [Arg "boolField" TyBool]], [T TokSmCol]),

    -- Testing parseStruct for parsing a struct with only a TyVoid field and no trailing tokens
    "Testing parseStruct with only a TyVoid field"
    ~: parseStruct [] [T TokStruct, T (TokIde "VoidOnlyStruct"), Braces [T (TokIde "voidField"), T TokCol, T TokTyVoid], T TokSmCol]
    ~?= Just ([Binding $ Struct "VoidOnlyStruct" [Arg "voidField" TyVoid]], [T TokSmCol]),

    -- Testing parseStruct for parsing a struct with a TyString field followed by more tokens
    "Testing parseStruct with a TyString field followed by other tokens"
    ~: parseStruct [] [T TokStruct, T (TokIde "StringAndMoreStruct"), Braces [T (TokIde "stringField"), T TokCol, T TokTyString, T TokCom, T (TokIde "nextField"), T TokCol, T TokTyBool], T TokSmCol]
    ~?= Just ([Binding $ Struct "StringAndMoreStruct" [Arg "stringField" TyString, Arg "nextField" TyBool]], [T TokSmCol])

  ]


testParseEnum :: Test
testParseEnum =
  TestList [
    "Testing parseEnum for nothing"
    ~: parseEnum [] [T TokEnum, T (TokIde "EnumWithNext"), T TokOpCrlBr, T (TokIde "element1"), T TokClCrlBr, T TokVar, T (TokIde "x"), T TokAsgn, T (TokLit (LitInt 42)), T TokSmCol]
    ~?= Nothing,
  
    "Testing parseEnum with no elements"
    ~: parseEnum [] [T TokEnum, T (TokIde "EmptyEnum"), Braces [], T TokSmCol]
    ~?= Just ([Binding $ Enum "EmptyEnum" []], [T TokSmCol]),

    "Testing parseEnum with single element"
    ~: parseEnum [] [T TokEnum, T (TokIde "SingleElementEnum"), Braces [T (TokIde "One")], T TokSmCol]
    ~?= Just ([Binding $ Enum "SingleElementEnum" [Bound "One" TyInt (Value (LitInt 0))]], [T TokSmCol]),

    "Testing parseEnum with multiple elements"
    ~: parseEnum [] [T TokEnum, T (TokIde "MultiElementEnum"), Braces [T (TokIde "One"), T TokCom, T (TokIde "Two")], T TokSmCol]
    ~?= Just ([Binding $ Enum "MultiElementEnum" [Bound "One" TyInt (Value (LitInt 0)), Bound "Two" TyInt (Value (LitInt 1))]], [T TokSmCol]),

    "Testing parseEnum with misplaced comma"
    ~: parseEnum [] [T TokEnum, T (TokIde "BadEnum"), Braces [T (TokIde "One"), T TokCom], T TokSmCol]
    ~?= Just ([Binding (Enum "BadEnum" [Bound "One" TyInt (Value (LitInt 0))])],[T TokSmCol]),

    "Testing parseEnum with subsequent tokens"
    ~: parseEnum [] [T TokEnum, T (TokIde "EnumWithSubsequentTokens"), Braces [T (TokIde "One")], T TokCom, T (TokIde "UnrelatedToken")]
    ~?= Just ([Binding $ Enum "EnumWithSubsequentTokens" [Bound "One" TyInt (Value (LitInt 0))]], [T TokCom, T (TokIde "UnrelatedToken")])
  ]

testParseTypeDecl :: Test
testParseTypeDecl = TestList [
    "Testing parseTypeDecl with String array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyStringArray"), T TokAsgn, T TokTyString, Hooks []]]
    ~?= Just ([Binding $ Type "MyStringArray" (TyArray TyString)], []),

    "Testing parseTypeDecl with Float array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyFloatArray"), T TokAsgn, T TokTyFloat, Hooks []]]
    ~?= Just ([Binding $ Type "MyFloatArray" (TyArray TyFloat)], []),

    "Testing parseTypeDecl with Char array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyCharArray"), T TokAsgn, T TokTyChar, Hooks []]]
    ~?= Just ([Binding $ Type "MyCharArray" (TyArray TyChar)], []),

    "Testing parseTypeDecl with simple String type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyString"), T TokAsgn, T TokTyString]]
    ~?= Just ([Binding $ Type "MyString" TyString], []),

    "Testing parseTypeDecl with simple Int type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyInt"), T TokAsgn, T TokTyInt]]
    ~?= Just ([Binding $ Type "MyInt" TyInt], []),

    "Testing parseTypeDecl with undefined type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyUnknown"), T TokAsgn, T (TokIde "UnknownType")]]
    ~?= Nothing,

    "Testing parseTypeDecl with incorrect declaration pattern"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T TokAsgn, T TokTyString, Hooks []]]
    ~?= Nothing,

    -- More test cases to cover other branches and edge cases can be added here.
    "Testing parseTypeDecl with incomplete declaration"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "Incomplete")]]
    ~?= Nothing,

    "Testing parseTypeDecl with extra tokens"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "Extra"), T TokAsgn, T TokTyBool, T (TokIde "Token")]]
    ~?= Nothing,

    "Testing parseTypeDecl with Int array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyIntArray"), T TokAsgn, T TokTyInt, Hooks []]]
    ~?= Just ([Binding $ Type "MyIntArray" (TyArray TyInt)], []),

    "Testing parseTypeDecl with Bool array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyBoolArray"), T TokAsgn, T TokTyBool, Hooks []]]
    ~?= Just ([Binding $ Type "MyBoolArray" (TyArray TyBool)], []),

    "Testing parseTypeDecl with Void array type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyVoidArray"), T TokAsgn, T TokTyVoid, Hooks []]]
    ~?= Just ([Binding $ Type "MyVoidArray" (TyArray TyVoid)], []),

    "Testing parseTypeDecl with simple Float type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyFloat"), T TokAsgn, T TokTyFloat]]
    ~?= Just ([Binding $ Type "MyFloat" TyFloat], []),

    "Testing parseTypeDecl with simple Char type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyChar"), T TokAsgn, T TokTyChar]]
    ~?= Just ([Binding $ Type "MyChar" TyChar], []),

    "Testing parseTypeDecl with simple Bool type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyBool"), T TokAsgn, T TokTyBool]]
    ~?= Just ([Binding $ Type "MyBool" TyBool], []),

    "Testing parseTypeDecl with simple Void type"
    ~: parseTypeDecl [] [BSection [T TokDeclType, T (TokIde "MyVoid"), T TokAsgn, T TokTyVoid]]
    ~?= Just ([Binding $ Type "MyVoid" TyVoid], [])
  ]

testParseIf :: Test
testParseIf = TestList [
    "Testing parseIf with correct if-else structure" 
    ~: parseIf [] [
        T TokIf, 
        Prths [T (TokIde "condition")], 
        Braces [T (TokIde "ifBody")], 
        T TokElse, 
        Braces [T (TokIde "elseBody")], 
        T TokSmCol
    ] 
    ~?= Just ([
        Control $ If 
            (CallId "condition") 
            [CallId "ifBody"] 
            [CallId "elseBody"]
    ], [T TokSmCol]),

    "Testing parseIf with correct if structure without else"
    ~: parseIf [] [
        T TokIf, 
        Prths [T (TokIde "condition")], 
        Braces [T (TokIde "ifBody")], 
        T TokSmCol
    ] 
    ~?= Just ([
        Control $ If 
            (CallId "condition") 
            [CallId "ifBody"] 
            [Empty]
    ], [T TokSmCol])
  ]

testParseReturn :: Test
testParseReturn = TestList [
    "Testing parseReturn for valid return statement" 
    ~: parseReturn [] [BSection [T TokReturn, T (TokIde "someValue")]] 
    ~?= Just ([Control (Return (CallId "someValue"))],[]),
  
    "Testing parseReturn for return followed by more code" 
    ~: parseReturn [] [BSection [T TokReturn, T (TokIde "someValue")], T (TokIde "x")] 
    ~?= Just ([Control (Return (CallId "someValue"))],[T (TokIde "x")]),

    "Testing parseReturn with no return value"
    ~: parseReturn [] [BSection [T TokReturn]] 
    ~?= Nothing,

    "Testing parseReturn with additional tokens after return"
    ~: parseReturn [] [BSection [T TokReturn, T (TokIde "someValue"), T TokSmCol, T (TokIde "x")]] 
    ~?= Nothing,

    "Testing parseReturn with incorrect return expression"
    ~: parseReturn [] [BSection [T TokReturn, T (TokIde "incorrect")]] 
    ~?= Just ([Control (Return (CallId "incorrect"))],[])
  ]

testParseFunctionCall :: Test
testParseFunctionCall = TestList [
    "Testing parseFunctionCall with no arguments" 
    ~: parseFunctionCall [] [T (TokIde "emptyFunction"), Prths []] 
    ~?= Just ([CallFun "emptyFunction" []], []),

    "Testing parseFunctionCall with single integer argument"
    ~: parseFunctionCall [] [T (TokIde "singleArgFunction"), Prths [T (TokLit (LitInt 1))]] 
    ~?= Just ([CallFun "singleArgFunction" [Value (LitInt 1)]], []),

    "Testing parseFunctionCall with multiple mixed arguments"
    ~: parseFunctionCall [] [T (TokIde "multiArgFunction"), Prths [T (TokLit (LitInt 1)), T TokCom, T (TokLit (LitString "arg2"))]] 
    ~?= Just ([CallFun "multiArgFunction" [Value (LitInt 1), Value (LitString "arg2")]], []),

    "Testing parseFunctionCall with complex argument expression"
    ~: parseFunctionCall [] [T (TokIde "complexFunction"), Prths [T (TokIde "x"), T TokAdd, T (TokLit (LitInt 10))]] 
    ~?= Just ([CallFun "complexFunction" [Operator Add (CallId "x") (Value (LitInt 10))]], []),

    "Testing parseFunctionCall with nested function call as argument"
    ~: parseFunctionCall [] [T (TokIde "nestedFunction"), Prths [T (TokIde "innerFunction"), Prths [T (TokLit (LitInt 5))]]] 
    ~?= Just ([CallFun "nestedFunction" [CallFun "innerFunction" [Value (LitInt 5)]]], []),

    "Testing parseFunctionCall with invalid argument syntax (missing comma)"
    ~: parseFunctionCall [] [T (TokIde "badSyntaxFunction"), Prths [T (TokLit (LitInt 1)), T (TokLit (LitInt 2))]] 
    ~?= Nothing,

    "Testing parseFunctionCall with improper nested parentheses"
    ~: parseFunctionCall [] [T (TokIde "improperNested"), Prths [T TokOpPrth, T (TokIde "x"), T TokClPrth]] 
    ~?= Nothing,

    "Testing parseFunctionCall with additional tokens after call"
    ~: parseFunctionCall [] [T (TokIde "functionWithTail"), Prths [T (TokLit (LitInt 1))], T TokSmCol] 
    ~?= Just ([CallFun "functionWithTail" [Value (LitInt 1)]], [T TokSmCol]),

    "Testing parseFunctionCall with a boolean literal as an argument"
    ~: parseFunctionCall [] [T (TokIde "boolArgFunction"), Prths [T (TokLit (LitBool True))]] 
    ~?= Just ([CallFun "boolArgFunction" [Value (LitBool True)]], [])
  ]
