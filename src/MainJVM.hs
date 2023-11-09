module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (replaceExtension, takeDirectory, takeBaseName)
import System.Process

import Instant.ErrM

import CompilerJVM
import Runner


run :: String -> RunFunction
run classNameString parser inputString =
    let instantString = instantLexer inputString in
    case parser instantString of
        Bad inputString -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr inputString
            exitFailure
        Ok programTree -> do
            compile programTree classNameString

runFile :: RunFileFunction
runFile parser file = do
    instantString <- readFile file
    let classNameString = takeBaseName file
    let jasminFile = replaceExtension file "j"
    let targetDirectory = takeDirectory file
    jasminString <- run classNameString parser instantString
    writeFile jasminFile jasminString
    (exitCode, outputMessage, errorMessage) <- readProcessWithExitCode ("java") ["-jar", "./lib/jasmin.jar", "-d", targetDirectory, jasminFile] ""
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
