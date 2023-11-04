module PreExecution (MainProg, optimize, managesEntryPoint) where


import Instruction (Instruction(..), Data(..), Insts, Prog)
import Data.List (isPrefixOf)


type MainProg = (Insts, Prog)


optimize :: Prog -> Prog
optimize prog =
    case managesEntryPoint prog of
        (_, p) -> p


managesEntryPoint :: Prog -> MainProg
managesEntryPoint [] = ([], [])
managesEntryPoint prog = managesEntryPoint' $ last prog
  where
    managesEntryPoint' :: (String, Insts) -> MainProg
    managesEntryPoint' (moduleName, insts)
        | "InterpreterLine" `isPrefixOf` moduleName =
            managesEntryPointForInterpreter (init prog) insts
        | otherwise =
            managesEntryPointCompiler prog


managesEntryPointForInterpreter :: Prog -> Insts -> MainProg
managesEntryPointForInterpreter prog main = (main ++ [Push PrintTop, Call], handleProg [] prog)


managesEntryPointCompiler :: Prog -> MainProg
managesEntryPointCompiler prog = ([PushFromEnv "main", Call], handleProg [] prog)


handleProg :: Prog -> Prog -> Prog
handleProg acc [] = acc
handleProg acc ((moduleName, moduleInsts) : next) =
    case removeEverythingUnnecessary [] moduleInsts of
        [   ] -> handleProg acc next
        insts -> handleProg (acc ++ [(moduleName, insts)]) next
  where
    removeEverythingUnnecessary :: Insts -> Insts -> Insts
    removeEverythingUnnecessary a [] = a
    removeEverythingUnnecessary a (SaveToEnv name insts : n) =
        removeEverythingUnnecessary (a ++ [SaveToEnv name insts]) n
    removeEverythingUnnecessary a (_ : n) =
        removeEverythingUnnecessary a n
