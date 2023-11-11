module CompilerJVM (compile) where

import qualified Data.Map as Map

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Control.Monad.State
import Control.Monad.Except

import Instant.Abs

------------------
-- types & data --
------------------
type Position = BNFC'Position
type ExpTreeHeight = Exp' (Position, Int)

data Error = Error Position String
    deriving (Show)

data CompilerState = CompilerState {
    variableIdentifiers :: Map.Map String Int,
    resultAccumulator   :: ShowS,
    stackSizeCurrent    :: Int,
    stackSizeMaximal    :: Int
}

type CompilerStatus = Except Error

type CompilerStateT = StateT CompilerState CompilerStatus

type ExpState  = CompilerStateT ()
type StmtState = CompilerStateT ()

---------------------
-- header & footer --
---------------------
header :: String -> Int -> Int -> String
header classNameString limitLocals limitStack = "\
    \.class public " ++ classNameString ++ "\n\
    \.super java/lang/Object\n\
    \\n\
    \.method public <init>()V\n\
    \    aload_0\n\
    \    invokespecial java/lang/Object/<init>()V\n\
    \    return\n\
    \.end method\n\
    \\n\
    \.method public static main([Ljava/lang/String;)V\n\
    \.limit locals " ++ show limitLocals ++ "\n\
    \.limit stack " ++ show limitStack ++ "\n\
    \"

footer :: String
footer = "\
    \    return\n\
    \.end method\n\
    \"

----------------------
-- helper functions --
----------------------
printPosition :: (Int, Int) -> String
printPosition (line, column) = (show line) ++ ":" ++ (show column)

decreaseStackSizeCurrent :: CompilerStateT ()
decreaseStackSizeCurrent = do
    currentSize <- gets stackSizeCurrent
    let decreasedSize = currentSize - 1
    modify (\state -> state {
        stackSizeCurrent = decreasedSize
    })

increaseStackSizeCurrent :: CompilerStateT ()
increaseStackSizeCurrent = do
    currentSize <- gets stackSizeCurrent
    let increasedSize = currentSize + 1
    modify (\state -> state {
        stackSizeCurrent = increasedSize,
        stackSizeMaximal = max (stackSizeMaximal state) increasedSize
    })

accumulate :: String -> CompilerStateT ()
accumulate addition =
    modify (\state -> state {
        resultAccumulator = resultAccumulator state . showString(
            "    " ++ addition ++ "\n"
        )
    })

-----------------------------------
-- program tree height functions --
-----------------------------------
treeHeight :: ExpTreeHeight -> Int
treeHeight (ExpAdd (_, height) _ _) = height
treeHeight (ExpSub (_, height) _ _) = height
treeHeight (ExpMul (_, height) _ _) = height
treeHeight (ExpDiv (_, height) _ _) = height
treeHeight (ExpLit (_, height) _) = height
treeHeight (ExpVar (_, height) _) = height

maxTreeHeight :: Int -> Int -> Int
maxTreeHeight treeHeightA treeHeightB =
    if treeHeightA > treeHeightB
        then max treeHeightA (treeHeightB + 1)
        else max treeHeightB (treeHeightA + 1)

setMaxTreeHeight :: Exp -> ExpTreeHeight
setMaxTreeHeight (ExpAdd location expressionL expressionR) =
    let expL = setMaxTreeHeight expressionL in
    let expR = setMaxTreeHeight expressionR in
    let maxHeight = maxTreeHeight (treeHeight expL) (treeHeight expR) in
    ExpAdd (location, maxHeight) expL expR

setMaxTreeHeight (ExpSub location expressionL expressionR) =
    let expL = setMaxTreeHeight expressionL in
    let expR = setMaxTreeHeight expressionR in
    let maxHeight = maxTreeHeight (treeHeight expL) (treeHeight expR) in
    ExpSub (location, maxHeight) expL expR

setMaxTreeHeight (ExpMul location expressionL expressionR) =
    let expL = setMaxTreeHeight expressionL in
    let expR = setMaxTreeHeight expressionR in
    let maxHeight = maxTreeHeight (treeHeight expL) (treeHeight expR) in
    ExpMul (location, maxHeight) expL expR

setMaxTreeHeight (ExpDiv location expressionL expressionR) =
    let expL = setMaxTreeHeight expressionL in
    let expR = setMaxTreeHeight expressionR in
    let maxHeight = maxTreeHeight (treeHeight expL) (treeHeight expR) in
    ExpDiv (location, maxHeight) expL expR

setMaxTreeHeight (ExpLit location value) = ExpLit (location, 1) value

setMaxTreeHeight (ExpVar location identifier) = ExpVar (location, 1) identifier

-----------------------
-- execute functions --
-----------------------
execute :: Stmt -> StmtState
execute (SAss _ (Ident identifier) expression) = do
    maybeValue <- gets (\state -> Map.lookup identifier (variableIdentifiers state))
    evaluate (setMaxTreeHeight expression)
    nextLocal <- maybe
        (
            do
                nextLoc <- gets (Map.size . variableIdentifiers)
                modify (\state -> state {
                    variableIdentifiers = Map.insert identifier nextLoc (variableIdentifiers state)
                })
                return nextLoc
        )
        return maybeValue
    let separator = if nextLocal <= 3 then "_" else " "
    accumulate ("istore" ++ separator ++ show nextLocal)
    decreaseStackSizeCurrent

execute (SExp _ expression) = do
    accumulate "getstatic java/lang/System/out Ljava/io/PrintStream;"
    increaseStackSizeCurrent
    evaluate (setMaxTreeHeight expression)
    accumulate "invokevirtual java/io/PrintStream/println(I)V"
    decreaseStackSizeCurrent
    decreaseStackSizeCurrent

------------------------
-- evaluate functions --
------------------------
evaluate :: ExpTreeHeight -> ExpState
evaluate (ExpLit _ value) = do
    increaseStackSizeCurrent
    case value of
        val |     -1 == val                 -> accumulate "iconst_m1"
        val |      0 <= val && val <= 5     -> accumulate ("iconst_" ++ show val)
        val |   -128 <= val && val <= 127   -> accumulate ("bipush " ++ show val)
        val | -32768 <= val && val <= 32767 -> accumulate ("sipush " ++ show val)
        val                                 -> accumulate ("ldc " ++ show val)


evaluate (ExpVar (position, _) (Ident identifier)) = do
    maybeValue <- gets (\state -> Map.lookup identifier (variableIdentifiers state))
    case maybeValue of
        Just value -> do
            let separator = if value <= 3 then "_" else " "
            accumulate ("iload" ++ separator ++ show value)
            increaseStackSizeCurrent
        Nothing -> do
            throwError error
    where
        error = Error position errorMessage
        errorMessage = "undefined variable: '" ++ identifier ++ "'"


evaluate (ExpAdd _ expressionL expressionR) = evaluateOperator "iadd" expressionL expressionR

evaluate (ExpSub _ expressionL expressionR) = evaluateOperator "isub" expressionL expressionR

evaluate (ExpMul _ expressionL expressionR) = evaluateOperator "imul" expressionL expressionR

evaluate (ExpDiv _ expressionL expressionR) = evaluateOperator "idiv" expressionL expressionR

evaluateOperator :: String -> ExpTreeHeight -> ExpTreeHeight -> ExpState
evaluateOperator operator expressionL expressionR = do
    let heightL = treeHeight expressionL
    let heightR = treeHeight expressionR
    if heightL < heightR
        then do
            evaluate expressionR
            evaluate expressionL
            if (operator == "isub" || operator == "idiv")
                then do
                    accumulate "swap"
                else do
                    return ()
        else do
            evaluate expressionL
            evaluate expressionR
    accumulate operator
    decreaseStackSizeCurrent



-----------------------
-- runtime functions --
-----------------------
executeProgram :: Program -> StmtState
executeProgram (Prog _ statementList) = mapM_ execute statementList

compile :: Program -> String -> IO String
compile program classNameString = do
    let initialVariableIdentifiers = Map.singleton "_args" 0
    let initialCompilerState = CompilerState initialVariableIdentifiers id 0 0
    let result = runExcept . flip execStateT initialCompilerState . executeProgram $ program
    case result of
        Right (CompilerState variableIdentifiers accumulator _ stackSizeMaximal) -> do
            return (showString (header classNameString (Map.size variableIdentifiers) stackSizeMaximal) . accumulator . showString footer $ "\n")
        Left (Error position errorMessage) -> do
            let positionMessage = case position of
                    Just pos -> printPosition pos
                    Nothing  -> "unknown position"
            hPutStrLn stderr "Error:"
            hPutStrLn stderr (positionMessage ++ ": " ++ errorMessage)
            exitFailure
