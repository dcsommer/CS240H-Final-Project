-- TODO this module will interpret a Program and save state about the
-- execution. The use case will be that you "run" a Program. Then you
-- query the resulting state.
module Interpreter() where

import MGDS

run :: Program -> Integer

runW :: Program -> SymTable -> Integer

run Constant x sym = x

run Var


run _ = undefined