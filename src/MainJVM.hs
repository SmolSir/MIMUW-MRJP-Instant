module Main where

import System.IO (stderr, hPutStrLn)
import System.FilePath (dropExtension)
import System.Exit (exitFailure, exitSuccess)

import Instant.ErrM

import CompilerJVM
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
            print programTree
            compile programTree
            exitSuccess

runFile :: RunFileFunction
runFile parser file = do
    fileContent <- readFile file
    putStrLn (dropExtension file)
    output <- run parser fileContent
    exitSuccess

main :: IO ()
main = mainRunner runFile
