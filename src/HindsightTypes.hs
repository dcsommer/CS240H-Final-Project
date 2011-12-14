module HindsightTypes where

import MGDS
import InterpreterTypes

-- the function, the args, and all calls with the env at that point
data CallTree = CallTree Function [Integer] [(CallTree, Env)]

instance Show CallTree where
    show (CallTree f args _) = (show f) ++ " " ++ (show args) ++ "\n"

type CallTrace = [(CallTree, Env)]

                      