module Instruction
  ( Data(..)
  , Instruction(..)
  , Args
  , Stack
  , Insts
  , Env
  , Prog
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
  | Not
  | Or
  | Gt
  | Lt
  | Eq
  -- Functions
  | Fun   Insts
  -- System
  | PrintTop
  | PrintArray
  | PrintAll
  deriving (Show, Eq)


data Instruction
  = Call
  | Ret
  | Exit        Int
  | Push        Data
  | JumpIfFalse Int
  | PushFromArg Int
  | SaveToEnv   String Insts
  | PushFromEnv String
  deriving (Show, Eq)


type Args  = [Data]
type Stack = [Data]
type Insts = [Instruction]
type Prog  = [(String, Insts)]
type Env   = [(String, Insts)]
