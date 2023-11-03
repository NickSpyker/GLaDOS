module BlockExprTest(testToBlock) where

import BlockExpr (BExpr(..), tokensToBlock)

import Lexer (Token(..), tokenize)
import Test.HUnit ( (~:), (~?=), Test(TestList) )
import Data (Literal(..))

testToBlock :: Test
testToBlock = 
  TestList
  [
    "Parse empty tokens"
    ~: tokensToBlock "testmodule" []
    ~?= Left "<Invalid end of file> #@#",
    "Parse simple module with no tokens"
    ~: tokensToBlock "testmodule" [TokVar]
    ~?= Right (BModule "testmodule" [T TokVar]),
    "Parse braces"
    ~: tokensToBlock "testmodule" [TokOpCrlBr, TokVar, TokClCrlBr]
    ~?= Right (BModule "testmodule" [Braces [T TokVar]]),
    "Missing closing brace"
    ~: tokensToBlock "testmodule" [TokOpCrlBr, TokVar]
    ~?= Left "<Expected '}'> #[TokVar]#",
    "Parse parentheses"
    ~: tokensToBlock "testmodule" [TokOpPrth, TokVar, TokClPrth]
    ~?= Right (BModule "testmodule" [Prths [T TokVar]]),
    "Missing closing parenthesis"
    ~: tokensToBlock "testmodule" [TokOpPrth, TokVar]
    ~?= Left "<Expected ')'> #[TokVar]#",
    "Parse hooks"
    ~: tokensToBlock "testmodule" [TokOpCrch, TokVar, TokClCrch]
    ~?= Right (BModule "testmodule" [Hooks [T TokVar]]),
    "Missing closing hook"
    ~: tokensToBlock "testmodule" [TokOpCrch, TokVar]
    ~?= Left "<Expected ']'> #[TokVar]#",
    "Parse semicolon"
    ~: tokensToBlock "testmodule" [TokVar, TokSmCol, TokVar]
    ~?= Right (BModule "testmodule" [BSection [T TokVar], T TokVar]),
    "Parse multiple tokens"
    ~: tokensToBlock "testmodule" [TokVar, TokVar]
    ~?= Right (BModule "testmodule" [T TokVar, T TokVar]),
    "Unfinished parse due to extra tokens"
    ~: tokensToBlock "testmodule" [TokVar, TokOpCrlBr]
    ~?= Left "<Invalid end of file> #@#",
    "Parse open curly bracket with error"
    ~: tokensToBlock "testmodule" [TokOpCrlBr, TokOpPrth]
    ~?= Left "<Invalid end of file> #@#",
    "Parse open parentheses with error"
    ~: tokensToBlock "testmodule" [TokOpPrth, TokOpCrlBr]
    ~?= Left "<Invalid end of file> #@#",
    "Parse open hook with error"
    ~: tokensToBlock "testmodule" [TokOpCrch, TokOpPrth]
    ~?= Left "<Invalid end of file> #@#",
    "Parse section with semicolon"
    ~: tokensToBlock "testmodule" [TokVar, TokSmCol, TokVar, TokSmCol]
    ~?= Right (BModule "testmodule" [BSection [T TokVar], BSection [T TokVar]]),
    "Parser recognizes some structure but leaves tokens unparsed"
      ~: tokensToBlock "testmodule" [TokOpCrlBr, TokClCrlBr, TokVar]
      ~?= Right (BModule "testmodule" [Braces [],T TokVar])
  ]