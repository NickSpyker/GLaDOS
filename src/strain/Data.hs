module Data (Literal(..)) where


data Literal
  = LitString String
  | LitFloat  Float
  | LitChar   Char
  | LitSChar  Char
  | LitInt    Int
  deriving (Show, Eq)
