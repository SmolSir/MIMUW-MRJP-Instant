module Runner (mainRunner, RunFunction, RunFileFunction, instantLexer) where

import System.Environment (getArgs)

import Instant.Abs
import Instant.ErrM
import Instant.Lex
import Instant.Par

type ParseFunction a = [Token] -> Err a
type RunFunction = ParseFunction (Program) -> String -> IO String
type RunFileFunction = ParseFunction (Program) -> FilePath -> IO ()

instantLexer = myLexer

mainRunner :: RunFileFunction -> IO ()
mainRunner fileRunner = do
    args <- getArgs
    case args of
        file -> mapM_ (fileRunner pProgram) file
