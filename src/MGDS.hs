{-# LANGUAGE MagicHash #-}
module MGDS where

import GHC.Prim

-- The last two arguments to each constructor is the line number and
-- column number where it appears in the file
data Expression = Constant Integer --Int# Int#
                | Var String --Int# Int#
                | Add Expression Expression --Int# Int#
                | Subtract Expression Expression --Int# Int#
                | Multiply Expression Expression --Int# Int#
                | Divide Expression Expression --Int# Int#
                | Equals Expression Expression --Int# Int#
                | Not Expression --Int# Int#
                | LogicalAnd Expression Expression --Int# Int#
                | LogicalOr Expression Expression --Int# Int#
                | Greater Expression Expression --Int# Int#
                | Less Expression Expression --Int# Int#
                | If Expression Expression Expression --Int# Int#
                | FunctionCall String [Expression] --Int# Int#
                  deriving Show
                  
-- A function consists of a name, a list of agument variable names, and an
-- expression for its body
data Function = Function {
     getName :: String,
     getParams :: [String],
     getExp :: Expression
    }
                deriving Show
                     

-- A program is simply a list of functions, one of which must be named
-- "main" 
newtype Program = Program [Function]
                  deriving Show