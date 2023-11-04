module CompilerLLVM (compile) where

import Instant.Abs


compile :: Program -> IO ()
compile program = do
    putStrLn "LLVM works!"
