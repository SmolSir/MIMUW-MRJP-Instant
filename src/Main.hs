module Main where

import System.IO (stdin, stderr, hGetContents, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad (when)

import Instant.Lex
import Instant.Par
import Instant.Print
import Instant.Abs
import Instant.ErrM

import Compiler


type ParseFunction a = [Token] -> Err a

instantLexer = myLexer

runFile :: ParseFunction (Program) -> FilePath -> IO ()
runFile parser file = readFile file >>= run parser

run :: ParseFunction (Program) -> String -> IO ()
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
main = do
    args <- getArgs
    case args of
        []   -> getContents >>= run pProgram
        file -> mapM_ (runFile pProgram) file
