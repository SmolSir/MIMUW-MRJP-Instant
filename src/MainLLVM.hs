module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)

import Instant.ErrM

import CompilerLLVM
import Runner


run :: RunFunction
run parser inputString =
    let instantString = instantLexer inputString in
    case parser instantString  of
        Bad inputString -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr inputString
            exitFailure
        Ok programTree -> do
            compile programTree
            exitSuccess

main :: IO ()
main = mainRunner run
