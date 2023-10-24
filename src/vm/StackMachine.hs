module StackMachine (Val(..), Op(..), Intsruction(..), Stack, Insts, exec, exeOp) where


data Val
  = Int Int
  | Bool Bool
  deriving (Show, Eq)


data Op
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | LessTo
  deriving (Show, Eq)


data Intsruction
  = Push Val
  | Call Op
  | Ret
  | JumpIfFalse Int
  deriving (Show, Eq)


type Stack = [Val]


type Insts = [Intsruction]


exec :: Insts -> Stack -> Either String Val
exec (Push value : next) stack = exec next $ value : stack
exec (Call op : next) stack =
  case exeOp op stack of
    Left err -> Left err
    Right re -> exec next re
exec (JumpIfFalse n : next) (Bool False : stack) = exec (drop n next) stack
exec (JumpIfFalse n : next) stack = exec next stack
exec (Ret : _) (res : _) = Right res


exeOp :: Op -> Stack -> Either String Stack
exeOp Div (Int x : Int 0 : _) = Left "Error: division by 0"
exeOp Eq (x : y : next) = Right $ Bool (x == y) : next
exeOp LessTo (Int x : Int y : next) = Right $ Bool (x < y) : next
exeOp op (Int x : Int y : next) = Right $ Int (x `applyOp` y) : next
  where
    applyOp :: Int -> Int -> Int
    applyOp =
      case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
exeOp op (a : b : _) =
  Left $
    "Error: invalid operation with " ++ show a ++ " and " ++ show b
