module Data (Literal(..)) where


data Literal
  = LitString String
  | LitFloat  Float
  | LitChar   Char
  | LitInt    Int
  deriving (Show, Eq)
