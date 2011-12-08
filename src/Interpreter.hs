-- TODO this module will interpret a Program and save state about the
-- execution. The use case will be that you "run" a Program. Then you
-- query the resulting state.
module Interpreter(run) where

import MGDS
import Data.IORef

-- types
type Env  = [(String, Integer)]
type FEnv = [Function]

getVar :: Env -> String -> Integer
getVar env var = maybe (-1) id (lookup var env)

getFunc :: FEnv -> String -> Function
getFunc fenv fname = head $ filter (\f -> (getName f) == fname) fenv

-- run a program
run :: Program -> Integer
run (Program fenv) = eval (getExp (last fenv)) [] fenv

-- eval an expression
eval :: Expression -> Env -> FEnv -> Integer

eval (Constant x) _ _ = x
eval (Var s) env _ = getVar env s    

eval (Add      e1 e2) env fenv = intFunc (+) e1 e2 env fenv
eval (Subtract e1 e2) env fenv = intFunc (-) e1 e2 env fenv
eval (Multiply e1 e2) env fenv = intFunc (*) e1 e2 env fenv
eval (Divide   e1 e2) env fenv = intFunc div e1 e2 env fenv

eval (Equals     e1 e2) env fenv = boolFunc (==) e1 e2 env fenv
eval (LogicalAnd e1 e2) env fenv = boolFunc (&&) e1 e2 env fenv
eval (LogicalOr  e1 e2) env fenv = boolFunc (||) e1 e2 env fenv

eval (Greater    e1 e2) env fenv = boolToInt $ intFunc (> ) e1 e2 env fenv
eval (Less       e1 e2) env fenv = boolToInt $ intFunc (< ) e1 e2 env fenv
                              
eval (Not e) env fenv = boolToInt $ (eval e env fenv) == 0
                        
eval (If cond e1 e2) env fenv = if (eval cond env fenv) /= 0
                                then (eval e1 env fenv)
                                else (eval e2 env fenv)
  
eval (FunctionCall fname es) env fenv =
    let vals = map (\e -> eval e env fenv) es
        f = getFunc fenv fname
        exp = getExp f
        env' = (zip (getParams f) vals) in
    eval exp env' fenv

-- helper eval functions
boolToInt :: Bool -> Integer
boolToInt False = 0
boolToInt True = 1
                 
intFunc :: (Integer -> Integer -> a) ->
           Expression -> Expression -> Env -> FEnv -> a
intFunc op e1 e2 env fenv = op (eval e1 env fenv)
                               (eval e2 env fenv)
  
boolFunc :: (Bool -> Bool -> Bool) ->
            Expression -> Expression -> Env -> FEnv -> Integer
boolFunc op e1 e2 env fenv =   
  if op (v1 /= 0) (v2 /= 0)
  then 1
  else 0
       where v1 = eval e1 env fenv
             v2 = eval e2 env fenv
  

