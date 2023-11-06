module CompilerJVM (compile) where

import qualified Data.Map as Map

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad.State
import Control.Monad.Except

import Instant.Abs

------------------
-- types & data --
------------------
type Position = BNFC'Position

data Error = Error Position String
    deriving (Show)

type CompilerState  = Map.Map Ident Integer
type CompilerStatus = ExceptT Error IO

type CompilerStateT = StateT CompilerState CompilerStatus

type ExpState  = CompilerStateT Integer
type StmtState = CompilerStateT ()

----------------------
-- helper functions --
----------------------
printPosition :: (Int, Int) -> String
printPosition (line, column) = (show line) ++ ":" ++ (show column)

-----------------------
-- execute functions --
-----------------------
execute :: Stmt -> StmtState
execute (SAss _ identifier expression) = do
    value <- evaluate expression
    modify (Map.insert identifier value)

execute (SExp _ expression) = do
    value <- evaluate expression
    liftIO (print value)

------------------------
-- evaluate functions --
------------------------
evaluate :: Exp -> ExpState
evaluate (ExpAdd _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    return (valueL + valueR)

evaluate (ExpSub _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    return (valueL - valueR)


evaluate (ExpMul _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    return (valueL * valueR)


evaluate (ExpDiv _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    return (div valueL valueR)


evaluate (ExpLit _ value) = return value


evaluate (ExpVar position identifier) = do
    value <- gets (Map.lookup identifier)
    case value of
        Just val -> return val
        Nothing  -> throwError error
    where
        error = Error position errorMessage
        errorMessage = "undefined variable: '" ++ show identifier ++ "'"

-----------------------
-- runtime functions --
-----------------------
executeProgram :: Program -> StmtState
executeProgram (Prog _ statementList) = mapM_ execute statementList

compile :: Program -> IO String
compile program = do
    result <- runExceptT . flip evalStateT Map.empty . executeProgram $ program
    case result of
        Right () -> exitSuccess
        Left  (Error position errorMessage) -> do
            let positionMessage = case position of
                    Just pos -> printPosition pos
                    Nothing  -> "unknown position"
            hPutStrLn stderr "Error:"
            hPutStrLn stderr (positionMessage ++ ": " ++ errorMessage)
            exitFailure
