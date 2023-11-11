module CompilerLLVM (compile) where

import qualified Data.Set as Set

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Control.Monad.State
import Control.Monad.Except

import Instant.Abs

------------------
-- types & data --
------------------
type Position = BNFC'Position

data Error = Error Position String
    deriving (Show)

data CompilerState = CompilerState {
    nextRegisterNumber  :: Int,
    variableIdentifiers :: Set.Set String,
    resultAccumulator   :: ShowS
}

type CompilerStatus = Except Error

type CompilerStateT = StateT CompilerState CompilerStatus

type ExpState  = CompilerStateT String
type StmtState = CompilerStateT ()

---------------------
-- header & footer --
---------------------
header :: String
header = "\
    \@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\
    \\n\
    \declare i32 @printf(i8*, ...)\n\
    \\n\
    \define void @printInt(i32 %x) {\n\
    \    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\
    \    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n\
    \    ret void\n\
    \}\n\
    \\n\
    \define i32 @main() {\n\
    \entry:\n\
    \"

footer :: String
footer = "\
    \    ret i32 0\n\
    \}\n\
    \"

----------------------
-- helper functions --
----------------------
printPosition :: Position -> String
printPosition Nothing = "[position unknown]"
printPosition (Just (line, column)) = "Ln " ++ show line ++ ", Col " ++ show column ++ "] "

nextRegister :: CompilerStateT String
nextRegister  = do
    registerNumber <- gets nextRegisterNumber
    modify (\state -> state { nextRegisterNumber = registerNumber + 1 })
    return ("%_" ++ show registerNumber)

accumulate :: String -> CompilerStateT ()
accumulate addition =
    modify (\state -> state {
        resultAccumulator = resultAccumulator state . showString(
            "    " ++ addition ++ "\n"
        )
    })

-----------------------
-- execute functions --
-----------------------
execute :: Stmt -> StmtState
execute (SAss _ (Ident identifier) expression) = do
    variableDefined <- gets (\state -> Set.member identifier (variableIdentifiers state))
    value <- evaluate expression
    if variableDefined
        then
            accumulate ("store i32 " ++ value ++ ", i32* %" ++ identifier)
        else do
            modify (\state -> state {
                variableIdentifiers = Set.insert identifier (variableIdentifiers state)
            })
            accumulate ("%" ++ identifier ++ " = alloca i32")
            accumulate ("store i32 " ++ value ++ ", i32* %" ++ identifier)

execute (SExp _ expression) = do
    value <- evaluate expression
    accumulate ("call void @printInt(i32 " ++ value ++ ")")

------------------------
-- evaluate functions --
------------------------
evaluateOperator :: String -> Exp -> Exp -> ExpState
evaluateOperator operator expressionL expressionR = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    register <- nextRegister
    accumulate (register ++ " = " ++ operator ++ " i32 " ++ valueL ++ ", " ++ valueR)
    return register

evaluate :: Exp -> ExpState
evaluate (ExpAdd _ expressionL expressionR) =
    evaluateOperator "add" expressionL expressionR

evaluate (ExpSub _ expressionL expressionR) =
    evaluateOperator "sub" expressionL expressionR

evaluate (ExpMul _ expressionL expressionR) =
    evaluateOperator "mul" expressionL expressionR

evaluate (ExpDiv _ expressionL expressionR) =
    evaluateOperator "sdiv" expressionL expressionR

evaluate (ExpLit _ value) = return (show value)

evaluate (ExpVar position (Ident identifier)) = do
    variableDefined <- gets (\state -> Set.member identifier (variableIdentifiers state))
    if variableDefined
        then do
            register <- nextRegister
            accumulate (register ++ " = load i32, i32* %" ++ identifier)
            return register
        else
            throwError error
    where
        error = Error position errorMessage
        errorMessage = "undefined variable: '" ++ identifier ++ "'"

-----------------------
-- runtime functions --
-----------------------
executeProgram :: Program -> StmtState
executeProgram (Prog _ statementList) = mapM_ execute statementList

compile :: Program -> IO String
compile program = do
    let initialVariableIdentifiers = Set.empty
    let initialCompilerState = CompilerState 0 initialVariableIdentifiers id
    let result = runExcept . flip execStateT initialCompilerState . executeProgram $ program
    case result of
        Right (CompilerState _ _ accumulator) -> do
            return (showString header . accumulator . showString footer $ "\n")
        Left (Error position errorMessage) -> do
            let positionMessage = printPosition position
            hPutStrLn stderr "Error:"
            hPutStrLn stderr (positionMessage ++ ": " ++ errorMessage)
            exitFailure
