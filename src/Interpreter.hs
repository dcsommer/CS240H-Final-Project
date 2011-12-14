-- TODO this module will interpret a Program and save state about the
-- execution. The use case will be that you "run" a Program. Then you
-- query the resulting state.
module Interpreter where

import MGDS
import Data.IORef
import HindsightTypes
import InterpreterTypes
import Data.Array

getVar :: Env -> String -> Integer
getVar env var = maybe (-1) id (lookup var env)

getFunc :: FEnv -> String -> Function
getFunc fenv fname = head $ filter (\f -> (getName f) == fname) fenv

-- run a program
run :: Program -> (Integer, [(Int, [CallTrace])])
run (Program fenv) = (val, lineMap)
    where main = last fenv
          (val, calls) = evalBody (getBody main) [] fenv
          tree = CallTree main [] calls
          traces = fenvToNode fenv (tree, [])
          lineMap = fmapToLineMap traces

fmapToLineMap :: [(Function, [CallTrace])] -> [(Int, [CallTrace])]
fmapToLineMap fm = foldl (++) [] $ map fmapToLineMap' fm 

fmapToLineMap' :: (Function, [CallTrace]) ->
                  [(Int, [CallTrace])]
fmapToLineMap' (Function _ _ _ ss, trace) = fmapToLineMap'' ss trace

fmapToLineMap'' :: [Statement] -> [CallTrace] -> [(Int, [CallTrace])]
fmapToLineMap'' [] _ = []
fmapToLineMap'' ((Assignment (LineInfo row col) _ _):ss) trace =
    (row, trace):(fmapToLineMap'' ss trace)
fmapToLineMap'' ((Return     (LineInfo row col) _  ):ss) trace =
    (row, trace):(fmapToLineMap'' ss trace)

fenvToNode :: FEnv -> (CallTree, Env) -> [(Function, [CallTrace])]
fenvToNode fenv tree = map (\f -> (f, funcToNode tree f)) fenv

funcToNode :: (CallTree, Env) -> Function -> [CallTrace]
funcToNode (tree@(CallTree nodeFunc _ subs), env) f =
    traces ++ (if f == nodeFunc
               then [[(tree, env)]]
               else [])
    where traces' = foldl (++) [] (map (\s -> funcToNode s f) subs)
          traces  = map (\t -> t ++ [(tree, env)]) traces'
                 
-- eval a list of statementsn
evalBody :: [Statement] -> Env -> FEnv -> (Integer, [(CallTree, Env)])
evalBody [] _ _ = undefined
evalBody ((Return _ exp):ss) env fenv = eval exp env fenv
evalBody (s:ss)              env fenv = (val, calls ++ calls')
    where (env', calls) = evalAssignment s env fenv
          (val, calls') = evalBody ss env' fenv

evalAssignment :: Statement -> Env -> FEnv -> (Env, [(CallTree, Env)])
evalAssignment (Assignment _ var exp) env fenv = (modifyEnv env var val, calls)
    where (val, calls) = eval exp env fenv

modifyEnv :: Env -> String -> Integer -> Env
modifyEnv [] var val = [(var, val)]
modifyEnv ((var', val'):bs) var val = if var == var'
                                      then (var, val):bs                  
                                      else (var', val'):(modifyEnv bs var val)


-- eval an expression
eval :: Expression -> Env -> FEnv -> (Integer, [(CallTree, Env)])

eval (Constant _ x) _ _ = (x, [])
eval (Var _ s) env _ = (getVar env s, [])

eval (Add      _ e1 e2) env fenv = intFunc (+) e1 e2 env fenv
eval (Subtract _ e1 e2) env fenv = intFunc (-) e1 e2 env fenv
eval (Multiply _ e1 e2) env fenv = intFunc (*) e1 e2 env fenv
eval (Divide   _ e1 e2) env fenv = intFunc div e1 e2 env fenv

eval (Equals     _ e1 e2) env fenv = boolFunc (==) e1 e2 env fenv
eval (LogicalAnd _ e1 e2) env fenv = boolFunc (&&) e1 e2 env fenv
eval (LogicalOr  _ e1 e2) env fenv = boolFunc (||) e1 e2 env fenv

eval (Greater    _ e1 e2) env fenv = (boolToInt val, calls)
    where (val, calls) = intFunc (> ) e1 e2 env fenv
                         
eval (Less       _ e1 e2) env fenv = (boolToInt val, calls)
    where (val, calls) = intFunc (< ) e1 e2 env fenv
                              
eval (Not _ e) env fenv = (boolToInt (val == 0), calls)
    where (val, calls) = eval e env fenv
                        
eval (If _ cond e1 e2) env fenv = if condVal /= 0
                                  then (thenVal, condCalls ++ thenCalls)
                                  else (elseVal, condCalls ++ elseCalls)
    where (condVal, condCalls) = eval cond env fenv
          (thenVal, thenCalls) = eval e1 env fenv
          (elseVal, elseCalls) = eval e2 env fenv
  
eval (FunctionCall _ fname es) env fenv =
    let (vals, calls) = evalExpressions es env fenv
        f = getFunc fenv fname
        body = getBody f
        env' = zip (getParams f) vals        
        (ret, calls') = evalBody body env' fenv in
    (ret, calls ++ [(CallTree f vals calls', env)])

-- helper eval functions
evalExpressions :: [Expression] -> Env -> FEnv -> ([Integer], [(CallTree, Env)])
evalExpressions [] _ _ = ([], [])
evalExpressions (e:es) env fenv = (val:vals, calls ++ calls')
    where 
      (val, calls) = eval e env fenv
      (vals, calls') = evalExpressions es env fenv

boolToInt :: Bool -> Integer
boolToInt False = 0
boolToInt True = 1
                 
intFunc :: (Integer -> Integer -> a) ->
           Expression -> Expression -> Env -> FEnv -> (a, [(CallTree, Env)])
intFunc op e1 e2 env fenv = (op v1 v2, calls1 ++ calls2)
    where (v1, calls1) = eval e1 env fenv
          (v2, calls2) = eval e2 env fenv
  
boolFunc :: (Bool -> Bool -> Bool) ->
            Expression -> Expression ->
            Env -> FEnv -> (Integer, [(CallTree, Env)])
boolFunc op e1 e2 env fenv =
  (if op (v1 /= 0) (v2 /= 0)
   then 1
   else 0,
   calls1 ++ calls2)
       where (v1, calls1) = eval e1 env fenv
             (v2, calls2) = eval e2 env fenv
  

