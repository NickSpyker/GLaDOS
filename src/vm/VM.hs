module VM (run, getNewProg) where


import PreExecution (managesEntryPoint)
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


getNewProg :: Prog -> Prog
getNewProg = buildEnv


buildEnv :: Prog -> Env
buildEnv [] = []
buildEnv ((_, insts) : next) = (buildEnv' insts) ++ (buildEnv next)
  where
    buildEnv' :: Insts -> Env
    buildEnv' [] = []
    buildEnv' (SaveToEnv name code : n) = (name, code) : buildEnv' n
    buildEnv' (_ : n) = buildEnv' n


--                       ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--                       ┃ 🎵 STACK MACHINE 🎵 ┃
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- ┃ Env:   Environment is used to store variables and functions     ┃
-- ┃ Args:  Arguments are literals available when calling a function ┃
-- ┃ Insts: Instructions manipulate the stack                        ┃
-- ┃ Stack: Stack stores data and operator                           ┃
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇


execute :: Env -> Args -> Insts -> Stack -> IO ()
execute env args insts stack = return ()
