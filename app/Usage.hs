module Usage (printHelp) where


printHelp :: IO ()
printHelp =
     putStrLn "USAGE"
  >> putStrLn "    ./glados [--interpret FILE(S) | FILE(S)]"
  >> putStrLn ""
  >> putStrLn "DESCRIPTION"
  >> putStrLn "    default              process FILE(S) compilation"
  >> putStrLn "    -h or --help         show this message"
  >> putStrLn "    -i or --interpret    interpret with FILE(S) input"
