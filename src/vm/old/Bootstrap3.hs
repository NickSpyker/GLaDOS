module Boostrap3 (Data(..), Instruction(..), Args, Stack, Insts, Env, launch, exec, fetchInEnv, exeOp) where


data Data
  -- Values
  = Int  Int
  | Bool Bool
  -- Operators
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | LessTo
  -- Functions
  | Fun Insts
  deriving (Show, Eq)


data Instruction
  = Push Data
  | Call
  | Ret
  | JumpIfFalse Int
  | PushArg Int
  | SaveEnv String Insts
  | PushEnv String
  deriving (Show, Eq)


type Args  = [Data]
type Stack = [Data]
type Insts = [Instruction]
type Env   = [(String, Insts)]


launch :: Args -> Insts -> IO ()
launch args insts =
  case exec args insts [] [] of
    Left  err    -> putStrLn $ "Error: " ++ err
    Right result -> print result


exec :: Args -> Insts -> Stack -> Env -> Either String Data
exec _ [] _ _ = Left "invalid end of function, should end with Call instruction"
exec args (Push value : next) stack env = exec args next (value : stack) env
exec args (Call : next) (d : stack) env =
  case exeOp d stack env of
    Left  err -> Left err
    Right re  -> exec args next re env
exec args (JumpIfFalse n : next) (Bool False : stack) env = exec args (drop n next) stack env
exec args (JumpIfFalse _ : next) stack env = exec args next stack env
exec args (PushArg index : next) stack env
  | length args <= index = Left $ "invalid arg call, try to get arg " ++ show index ++ ", with only " ++ show (length args) ++ " arguments"
  | otherwise = exec args next (args !! index : stack) env
exec _ (Ret : _) (result : _) _ = Right result
exec args (SaveEnv name ctt : next) stack env = exec args next stack ((name, ctt) : env)
exec args (PushEnv name : next) stack env =
  case fetchInEnv name env of
    Left  err -> Left err
    Right ctt -> exec args next (Fun ctt : stack) env
exec _ ints _ _ = Left $ "invalid instructions " ++ show ints


fetchInEnv :: String -> Env -> Either String Insts
fetchInEnv name [] = Left $ name ++ " not in bound"
fetchInEnv name ((fname, ctt) : next)
  | name == fname = Right ctt
  | otherwise     = fetchInEnv name next


exeOp :: Data -> Stack -> Env -> Either String Stack
exeOp (Fun insts) stack env =
  case exec stack insts [] env of
    Left  err    -> Left err
    Right output -> Right [output]
exeOp Div    (Int  _ : Int  0 :    _) _ = Left "division by 0"
exeOp Eq     (Int  x : Int  y : next) _ = Right $ Bool (x == y) : next
exeOp Eq     (Bool x : Bool y : next) _ = Right $ Bool (x == y) : next
exeOp LessTo (Int  x : Int  y : next) _ = Right $ Bool (x <  y) : next
exeOp op     (Int  x : Int  y : next) _ =
  case getOp of
    Left  err -> Left err
    Right vop -> Right $ Int (x `vop` y) : next
  where
    getOp :: Either String (Int -> Int -> Int)
    getOp =
      case op of
        Add -> Right (+)
        Sub -> Right (-)
        Mul -> Right (*)
        Div -> Right div
        _   -> Left $ "invalid operator " ++ show op
exeOp _ (a : b : _) _ = Left $ "invalid operation with " ++ show a ++ " and " ++ show b
exeOp _ stack _       = Left $ "invalid number of Call arguments, got " ++ show (length stack) ++ ", but expected 2"
