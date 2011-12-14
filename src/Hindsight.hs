module Hindsight where

import MGDS

data CallTree = CallTree Function [Integer] [CallTree]              
                deriving Show

type CallTrace = [CallTree]
      