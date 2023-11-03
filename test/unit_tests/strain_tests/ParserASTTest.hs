module ParserASTTest (testParse) where
import Lexer (Token (..))
import Test.HUnit
import BlockExpr(BExpr(..))
import ParserAST(buildASTtree, Ast(..), Control(..), Operator(..))
import Data (Literal(..))

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
        ~?= Right (Module "testmodule" [Operator Eq (Value (LitInt 5)) (Value (LitInt 3))])
    ]