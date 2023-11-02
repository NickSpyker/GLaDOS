module Data (Literal(..)) where


data Literal
  = LitString  String
  | LitFloat   Float
  | LitBool    Bool
  | LitChar    Char
  | LitSChar   Char
  | LitInt     Int
  | LitArray  [Literal]
  deriving (Show, Eq)
