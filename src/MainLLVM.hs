module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (replaceExtension)
import System.Process

import Instant.ErrM

import CompilerLLVM
import Runner


run :: RunFunction
run parser input =
    let tokenList = instantLexer input in
    case parser tokenList of
        Bad _ -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr input
            exitFailure
        Ok programTree -> do
            compile programTree

runFile :: RunFileFunction
runFile parser file = do
    instantCode <- readFile file
    let llvmFile = replaceExtension file "ll"
    let llvmBitcodeFile = replaceExtension file "bc"
    llvmCode <- run parser instantCode
    writeFile llvmFile llvmCode
    (exitCode, outputMessage, errorMessage) <-
        readProcessWithExitCode
            ("llvm-as")
            ["-o", llvmBitcodeFile, llvmFile]
            ""
    case exitCode of
        ExitSuccess ->
            exitSuccess
        ExitFailure _ -> do
            hPutStrLn stderr ("Error (program exited with code: " ++ show exitCode ++ ")")
            hPutStrLn stderr outputMessage
            hPutStrLn stderr errorMessage
            exitFailure

main :: IO ()
main = mainRunner runFile
