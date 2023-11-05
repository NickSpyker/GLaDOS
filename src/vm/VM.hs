module VM (run, optimizeProgram) where


import PreExecution (managesEntryPoint)
import VMoutput (output)
import Instruction
  ( Data(..)
  , Instruction(..)
  , Args
  , Stack
  , Insts
  , Env
  , Prog
  )


run :: Prog -> IO ()
run [] = return ()
run prog =
  case managesEntryPoint prog of
    ([], _) -> putStrLn "nothing to do"
    (main, []) -> execute [] [] main []
    (main, modules) -> execute (buildEnv modules) [] main []


optimizeProgram :: Prog -> Prog
optimizeProgram [] = []
optimizeProgram ((moduleName, SaveToEnv name code : _) : next) = (moduleName, [SaveToEnv name code]) : optimizeProgram next
optimizeProgram (_ : next) = optimizeProgram next


buildEnv :: Prog -> Env
buildEnv [] = []
buildEnv ((_, [SaveToEnv name code]) : next) = (name, code) : buildEnv next
buildEnv (_ : next) = buildEnv next


--                       ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--                       ┃ 🎵 STACK MACHINE 🎵 ┃
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- ┃ Env:   Environment is used to store variables and functions     ┃
-- ┃ Args:  Arguments are literals available when calling a function ┃
-- ┃ Insts: Instructions manipulate the stack                        ┃
-- ┃ Stack: Stack stores data and operator                           ┃
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇


execute :: Env -> Args -> Insts -> Stack -> IO ()
execute _   _    []                  _                = return ()
execute env args (PrintTop   : next) stack            = output [PrintTop]   stack >> execute env args next stack
execute env args (PrintStack : next) stack            = output [PrintStack] stack >> execute env args next stack
execute env args (Push     v : next) stack            = execute env args next (v : stack)
execute env args (Call       : next) (Fun insts : ns) = execute env [] insts [] >> execute env args next ns
execute env args (Call       :  ine) (o : sne)        =
  case call o sne of
    Left  err      -> putStrLn ("Error: " ++ err)
    Right newStack -> execute env args ine newStack
execute env args (SaveToEnv   n i  : next) stack = execute (env ++ [(n, i)]) args next stack
execute env args (PushFromEnv name : next) stack =
  case fetchEnv env name next of
    Left  err      -> putStrLn err
    Right newInsts -> execute env args newInsts stack
execute env args (PushFromArg _ : next) stack = execute env args next stack
execute _ _ _ _ = return ()


fetchEnv :: Env -> String -> Insts -> Either String Insts
fetchEnv ((envName, insts) : next) name oldInsts
  | envName == name = Right $ insts ++ oldInsts
  | otherwise       = fetchEnv next name oldInsts
fetchEnv _ name _   = Left $ "Not in scope: " ++ name


call :: Data -> Stack -> Either String Stack
call Mod (Int 0 :            _) = Left "divide by zero"
call Div (Int 0 :            _) = Left "divide by zero"
call Eq  (Int x : Int y : next) = Right $ Bool (x == y) : next
call Gt  (Int x : Int y : next) = Right $ Bool (x <  y) : next
call Lt  (Int x : Int y : next) = Right $ Bool (x >  y) : next
call op  (Int x : Int y : next) =
  case getOp of
    Left  err -> Left err
    Right vop -> Right $ Int (y `vop` x) : next
  where
    getOp :: Either String (Int -> Int -> Int)
    getOp = case op of
      Add -> Right (+)
      Sub -> Right (-)
      Mul -> Right (*)
      Div -> Right div
      Mod -> Right mod
      _   -> Left $ "invalid operator " ++ show op ++ " for two integer " ++ show x ++ " and " ++ show y
call Mod (Float 0.0 :              _) = Left "divide by zero"
call Div (Float 0.0 :              _) = Left "divide by zero"
call Eq  (Float   x : Float y : next) = Right $ Bool (x == y) : next
call Gt  (Float   x : Float y : next) = Right $ Bool (x <  y) : next
call Lt  (Float   x : Float y : next) = Right $ Bool (x >  y) : next
call op  (Float   x : Float y : next) =
  case getOp of
    Left  err -> Left err
    Right vop -> Right $ Float (y `vop` x) : next
  where
    getOp :: Either String (Float -> Float -> Float)
    getOp = case op of
      Add -> Right (+)
      Sub -> Right (-)
      Mul -> Right (*)
      Div -> Right (/)
      Mod -> Right (\ a b -> a - intToFloat (floor (a / b)) * b)
      _   -> Left $ "invalid operator " ++ show op ++ " for two integer " ++ show x ++ " and " ++ show y
    intToFloat :: Int -> Float
    intToFloat = fromIntegral
call Eq  (Bool x : Bool y : next) = Right $ Bool (x == y) : next
call And (Bool x : Bool y : next) = Right $ Bool (x && y) : next
call Or  (Bool x : Bool y : next) = Right $ Bool (x || y) : next
call Not (Bool x :          next) = Right $ Bool (not  x) : next
call Not (x :     _) = Left $ "invalid operation Not for " ++ show x
call op  (x : y : _) = Left $ "invalid operation " ++ show op ++ " for " ++ show x ++ " and " ++ show y
call _   _           = Left "invalid operation, need arguments"
