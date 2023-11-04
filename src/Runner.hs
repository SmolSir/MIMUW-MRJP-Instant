module Runner (mainRunner, Err(..), RunFunction, instantLexer) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Instant.Abs
import Instant.Lex
import Instant.Par
import Instant.Print
import Instant.ErrM

type ParseFunction a = [Token] -> Err a
type RunFunction = ParseFunction (Program) -> String -> IO ()

instantLexer = myLexer

runFile :: ParseFunction (Program) -> RunFunction -> FilePath -> IO ()
runFile parser runner file = readFile file >>= runner parser

mainRunner :: RunFunction -> IO ()
mainRunner runner = do
    args <- getArgs
    case args of
        []   -> getContents >>= runner pProgram
        file -> mapM_ (runFile pProgram runner) file
