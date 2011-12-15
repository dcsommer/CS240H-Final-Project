module Hindsight where

import MGDS
import Interpreter
import InterpreterTypes
import HindsightTypes
import System.IO

type LineMap = [(Int, [CallTrace])]

showFullState :: LineMap -> Int -> FEnv -> IO ()
showFullState map line fenv = 
  case lookup line map of
    Just traces -> do
      trace <- chooseTrace traces
      showFullTrace trace line fenv
    Nothing -> putStrLn "That is not a valid line of code to inspect"

showShortState :: LineMap -> Int -> FEnv -> IO ()
showShortState map line fenv = do
  case lookup line map of
    Just traces -> do
      trace <- chooseTrace traces
      showFrame (fst (head trace)) line fenv
    Nothing -> putStrLn "That is not a valid line of code to inspect"

showFullTrace :: CallTrace -> Int -> FEnv -> IO ()
showFullTrace ts@((t, _):_) line fenv = do
  showParentTrace (reverse ts)
  showFrame t line fenv

showParentTrace :: CallTrace -> IO ()
showParentTrace [] = undefined
showParentTrace (a:[]) = return ()
showParentTrace ((CallTree f _ _, _):
                 (ts@((_, env):_))) = do
  putStrLn (getName f)
  showBindings env
  putStrLn ""
  showParentTrace ts
                 
showFrame :: CallTree -> Int -> FEnv -> IO ()
showFrame (CallTree f@(Function _ fname _ _) args _) line fenv = do
  putStrLn fname
  showBindings $ bindingsToPoint f line args fenv

showBindings :: Env -> IO ()
showBindings [] = return ()
showBindings ((var, val):bs) = do
    putStrLn $ "  " ++ (show var) ++ " = " ++ (show val)
    showBindings bs

bindingsToPoint :: Function -> Int -> [Integer] -> FEnv -> Env
bindingsToPoint (Function _ _ params ss) line args fenv =
    evalBody' (bodyToPoint ss line) (zip params args) fenv

bodyToPoint :: [Statement] -> Int -> [Statement]
bodyToPoint [] _ = []
bodyToPoint (a@(Assignment (LineInfo row col) _ _):ss) row' =
    if row >= row'
    then []
    else a:(bodyToPoint ss row')
         
bodyToPoint (r@(Return   (LineInfo row col) _ ):ss) row' =
    if row >= row'
    then []
    else r:(bodyToPoint ss row')

bodyToPoint (r@(ReturnIf (LineInfo row col) _ _):ss) row' =
    if row >= row'
    then []
    else r:(bodyToPoint ss row')

evalBody' :: [Statement] -> Env -> FEnv -> Env
evalBody' [] env _ = env
evalBody' ((Return _ exp):ss) env fenv = env
evalBody' ((ReturnIf _ cond exp):ss) env fenv =
    if val /= 0
    then env
    else evalBody' ss env fenv
        where (val, _) = eval cond env fenv
evalBody' (s:ss)              env fenv = evalBody' ss env' fenv
    where (env', _) = evalAssignment s env fenv


chooseTrace :: [CallTrace] -> IO CallTrace
chooseTrace traces =
  if (length traces) == 1
  then return $ head traces
  else do
    putStr $ "This line of code has been executed " ++ (show $ length traces) ++
               " times.  Please choose one: "
    hFlush stdout
    input <- getLine
    let i = read input
    return $ traces !! (i - 1)

                 