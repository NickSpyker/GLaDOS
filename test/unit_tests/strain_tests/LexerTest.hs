module LexerTest (testTokenize) where

import Lexer (Token(..), tokenize)
import Test.HUnit ( (~:), (~?=), Test(TestList) )
import Data (Literal(..))

testTokenize :: Test
testTokenize =
  TestList
    [ "Basics valid tokenize tests"
        ~: tokenize "(let x = 5)"
        ~?= Right [TokOpPrth, TokVar, TokIde "x", TokAsgn, TokLit (LitInt 5), TokClPrth],
      "Testing valid multi-character tokens to tokenize"
        ~: tokenize "<= >= < > !="
        ~?= Right [TokLtEq, TokGtEq, TokLt, TokGt, TokNotEq],
      "Testing an invalid token to tokenize"
        ~: tokenize "(let $ = 5)"
        ~?= Left "<Invalid token> # $ = 5)#",
      "Testing valid float expression to tokenize"
        ~: tokenize "1.56"
        ~?= Right [TokLit (LitFloat 1.56)],
      "Testing valid alphanumeric expression to tokenize"
        ~: tokenize "A a"
        ~?= Right [TokIde "A", TokIde "a"],
      "Testing valid single operators to tokenize"
        ~: tokenize "+ - * / %"
        ~?= Right [TokAdd, TokSub, TokMul, TokDiv, TokMod],
      "Testing valid boolean logic to tokenize"
        ~: tokenize "! and or"
        ~?= Right [TokNot, TokAnd, TokOr],
      "Testing Just multi token"
        ~: tokenize "<="
        ~?= Right [TokLtEq],
      "Testing valid input with different sep"
        ~: tokenize "(let x\t= 5\n+)"
        ~?= Right [TokOpPrth, TokVar, TokIde "x", TokAsgn, TokLit (LitInt 5), TokAdd, TokClPrth],
      "Testing tokenizing valid strings with quotes"
        ~: tokenize "\"This is a string\""
        ~?= Right [TokLit (LitString "This is a string")],
      "Testing tokenizing a string that doesn't close with a quote"
        ~: tokenize "\"Incomplete String"
        ~?= Left "<Invalid token> #\"Incomplete String#",
      "Testing tokenizing an empty string"
        ~: tokenize ""
        ~?= Right [],
      "Testing tokenizing unmatched quotation mark"
        ~: tokenize "\"hello"
        ~?= Left "<Invalid token> #\"hello#",
      "Testing matched quotation marks"
        ~: tokenize "\"hello\""
        ~?= Right [TokLit (LitString "hello")],
      "Testing single character tokens" ~: 
        tokenize "( ) { } [ ] ; : . , \\ + - * / % = > <" 
        ~?= Right [TokOpPrth, TokClPrth, TokOpCrlBr, TokClCrlBr, TokOpCrch, TokClCrch, TokSmCol, TokCol, TokDot, TokCom, TokBckSlsh, TokAdd, TokSub, TokMul, TokDiv, TokMod, TokAsgn, TokGt, TokLt],
      "Testing compound tokens" ~: 
        tokenize ":: -> >= <= == != += *= -= /= "
        ~?= Right [TokDCol, TokRetTy, TokGtEq, TokLtEq, TokEq, TokNotEq, TokAddAsgn, TokMulAsgn, TokSubAsgn, TokDivAsgn],
       "Testing keywords" ~: 
        tokenize "not and or fun let const for if else in loop while break return continue string float bool void char int type struct enum use as"
        ~?= Right [TokNot, TokAnd, TokOr, TokFun, TokVar, TokConst, TokFor, TokIf, TokElse, TokIn, TokLoop, TokWhile, TokBreak, TokReturn, TokContinue, TokTyString, TokTyFloat, TokTyBool, TokTyVoid, TokTyChar, TokTyInt, TokDeclType, TokStruct, TokEnum, TokImport, TokRName],
      "Testing literals" ~: 
        tokenize "123 456.789 'c'" 
        ~?= Right [TokLit (LitInt 123), TokLit (LitFloat 456.789), TokLit (LitChar 'c')],
       "Testing identifiers" ~: 
        tokenize "ident1 ident2"
        ~?= Right [TokIde "ident1", TokIde "ident2"],
      "Testing boolean literals" ~: 
        tokenize "true false" 
        ~?= Right [TokLit (LitBool True), TokLit (LitBool False)],
        "Testing integer literals at end of input" ~: 
        tokenize "1234" 
        ~?= Right [TokLit (LitInt 1234)],
      "Testing float literals at end of input" ~: 
        tokenize "1234.56" 
        ~?= Right [TokLit (LitFloat 1234.56)],
      "Testing escaped single char literals" ~: 
        tokenize "'\\c'" 
        ~?= Right [TokLit (LitSChar 'c')],
      "Testing float literals not at end of input" ~: 
        tokenize "1234.56," 
        ~?= Right [TokLit (LitFloat 1234.56), TokCom],
      "Testing identifier with underscore" ~: 
        tokenize "var_name" 
        ~?= Right [TokIde "var_name"],
      "Testing identifier ending with underscore" ~: 
        tokenize "var_" 
        ~?= Right [TokIde "var_"],
      "Testing a single-line comment" ~: 
        tokenize "// This is a comment\n1234" 
        ~?= Right [TokLit (LitInt 1234)],
      "Testing tokens after a single-line comment" ~: 
        tokenize "var // This is a comment\n1234" 
        ~?= Right [TokIde "var", TokLit (LitInt 1234)],
      -- "Testing empty input for literal parsing" ~: 
      --   tokenize "" 
      --   ~?= Left "<Invalid token> ##",
      -- "Testing invalid float ending"
      --   ~: tokenize "1.2." 
      --   ~?= Left "<Invalid token> #1.2.#",
      "Testing invalid starting character for literal parsing" ~: 
        tokenize "@" 
        ~?= Left "<Invalid token> #@#"
      -- "Testing mixed alphabets and numbers" ~: 
        -- tokenize "12a34" 
        -- ~?= Left "<Invalid token> #12a34#"
    ]
