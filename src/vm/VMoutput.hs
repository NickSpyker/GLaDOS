module VMoutput (output) where


import Instruction (Stack, Insts, Instruction(..), Data(..))


output :: Insts -> Stack -> IO ()
output (PrintTop   : _) (ArrayEnd : array) = printArray [] array
output (PrintTop   : _) (top      :     _) = printData top
output (PrintStack : _) stack              = putStr "▇ " >> printStack (reverse stack)
output _                _                  = return ()


printArray :: Stack -> Stack -> IO ()
printArray []  (ArrayStart :    _) = return ()
printArray acc (top        : next) = printArray (top : acc) next
printArray []  _                   = return ()
printArray acc _                   = printLoop acc
  where
    printLoop :: Stack -> IO ()
    printLoop []              = return ()
    printLoop (Char c : next) = putChar   c >> printLoop next
    printLoop (d      : next) = printData d >> printLoop next


printStack :: Stack -> IO ()
printStack []           = return ()
printStack (top : next) = putStr (show top) >> putStr " ┃ " >> printStack next


printData :: Data -> IO ()
printData (Float v)     = putStr $ show v
printData (Bool  True)  = putStr "true"
printData (Bool  False) = putStr "false"
printData (Char  v)     = putChar v
printData (Int   v)     = putStr $ show v
printData _             = return ()
