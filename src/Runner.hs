module Runner (mainRunner, Err(..), RunFunction, RunFileFunction, instantLexer) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Instant.Abs
import Instant.Lex
import Instant.Par
import Instant.Print
import Instant.ErrM

type ParseFunction a = [Token] -> Err a
type RunFunction = ParseFunction (Program) -> String -> IO String
type RunFileFunction = ParseFunction (Program) -> FilePath -> IO ()

instantLexer = myLexer

mainRunner :: RunFileFunction -> IO ()
mainRunner fileRunner = do
    args <- getArgs
    case args of
        -- []   -> getContents >>= runner pProgram
        file -> mapM_ (fileRunner pProgram) file
