{-# LANGUAGE OverloadedStrings #-}
module MGDS where

import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as P8

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

data Expression = Constant Integer
                | Var String
                | Add Expression Expression
                | Subtract Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                | Equals Expression Expression
                | Not Expression
                | LogicalAnd Expression Expression
                | LogicalOr Expression Expression
                | Greater Expression Expression
                | Less Expression Expression
                | If Expression Expression Expression
                | FunctionCall String [Expression]
                  
-- A function consists of a name, a list of agument variable names, and an
-- expression for its body
data Function = Function String [String] Expression

-- A program is simply a list of functions, one of which must be named
-- "main" 
newtype Program = Program [Function]