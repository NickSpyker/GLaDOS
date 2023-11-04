module VM (run) where


import PreExecution (MainProg, managesEntryPoint)
import Data.List (isPrefixOf)
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
    ([], _) ->
      putStrLn "nothing to do"
    (main, []) ->
      execute [] [] main []
    (main, modules) ->
      execute (buildEnv modules) [] main []


buildEnv :: Prog -> Env
buildEnv [] = []
buildEnv ((_, insts) : next) = (buildEnv' insts) ++ (buildEnv next)
  where
    buildEnv' :: Insts -> Env
    buildEnv' [] = []
    buildEnv' (SaveToEnv name code : n) = (name, code) : buildEnv' n


execute :: Env -> Args -> Insts -> Stack -> IO ()
execute env args insts stack =
     putStrLn ("ENV :\n\t" ++ show   env ++ "\n")
  >> putStrLn ("MAIN:\n\t" ++ show insts ++ "\n")
