module Instruction
  ( Data(..)
  , Instruction(..)
  , Args
  , Stack
  , Insts
  , Env
  ) where


data Data
  -- Values
  = Float Float
  | Bool  Bool
  | Char  Char
  | Int   Int
  -- Array start/end point
  | ArrayStart
  | ArrayEnd
  -- Operators
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Gt
  | Lt
  | Eq
  -- Functions
  | Fun
  deriving (Show, Eq)


data Instruction
  = Call
  | Ret
  | Push        Data
  | JumpIfFalse Int
  | PushArg     Int
  | SaveToEnv   String Insts
  | CallFromEnv String
  deriving (Show, Eq)


type Args  = Stack

type Stack = [Data]

type Insts = [Instruction]

type Env   = [(String, Insts)]
