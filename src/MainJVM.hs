module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (replaceExtension, takeDirectory, takeBaseName)
import System.Process

import Instant.ErrM

import CompilerJVM
import Runner


run :: String -> RunFunction
run className parser input =
    let tokenList = instantLexer input in
    case parser tokenList of
        Bad _ -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr input
            exitFailure
        Ok programTree -> do
            compile programTree className

runFile :: RunFileFunction
runFile parser file = do
    instantCode <- readFile file
    let className = takeBaseName file
    let jasminFile = replaceExtension file "j"
    let targetDirectory = takeDirectory file
    jasminCode <- run className parser instantCode
    writeFile jasminFile jasminCode
    (exitCode, outputMessage, errorMessage) <-
        readProcessWithExitCode
            ("java")
            ["-jar", "./lib/jasmin.jar", "-d", targetDirectory, jasminFile]
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
