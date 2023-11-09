module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (replaceExtension)
import System.Process

import Instant.ErrM

import CompilerLLVM
import Runner


run :: RunFunction
run parser inputString =
    let instantString = instantLexer inputString in
    case parser instantString of
        Bad inputString -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr inputString
            exitFailure
        Ok programTree -> do
            compile programTree
            exitSuccess

runFile :: RunFileFunction
runFile parser file = do
    instantString <- readFile file
    let llvmFile = replaceExtension file "ll"
    let llvmBitcodeFile = replaceExtension file "bc"
    llvmString <- run parser instantString
    writeFile llvmFile llvmString
    (exitCode, outputMessage, errorMessage) <- readProcessWithExitCode ("llvm-as") ["-o", llvmBitcodeFile, llvmFile] ""
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
