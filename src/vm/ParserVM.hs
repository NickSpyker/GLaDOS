module ParserVM (build) where


import Instruction (Insts)
import ParserAST (Ast(..))


build :: Ast -> Either String Insts
build _ = Left ""
