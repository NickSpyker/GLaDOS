module Data (Literal(..)) where


data Literal
  = LitString String
  | LitFloat  Float
  | LitBool   Bool
  | LitChar   Char
  | LitSChar  Char
  | LitInt    Int
  deriving (Show, Eq)
