module Usage (printHelp) where


printHelp :: IO ()
printHelp =
     putStrLn "USAGE"
  >> putStrLn "    ./glados [-i FILE(S) | FILE(S) | build FILE(S) | run OBJFILE(S)]"
  >> putStrLn ""
  >> putStrLn "DESCRIPTION"
  >> putStrLn "    default              process FILE(S)"
  >> putStrLn "    build                compile FILE(S)"
  >> putStrLn "    run                  run compiled OBJFILE(S)"
  >> putStrLn "    show                 display in a human-readable binary format"
  >> putStrLn "    -h or --help         show this message"
  >> putStrLn "    -i or --interpret    interpret with FILE(S) input"
