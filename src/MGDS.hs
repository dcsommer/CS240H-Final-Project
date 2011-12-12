{-# LANGUAGE BangPatterns #-}
module MGDS where

import GHC.Prim

--Fields are row, column
data LineInfo = LineInfo {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                deriving Show

-- The last two arguments to each constructor is the line number and
-- column number where it appears in the file
data Expression = Constant LineInfo Integer
                | Var LineInfo String
                | Add LineInfo Expression Expression
                | Subtract LineInfo Expression Expression
                | Multiply LineInfo Expression Expression
                | Divide LineInfo Expression Expression
                | Equals LineInfo Expression Expression
                | Not LineInfo Expression
                | LogicalAnd LineInfo Expression Expression
                | LogicalOr LineInfo Expression Expression
                | Greater LineInfo Expression Expression
                | Less LineInfo Expression Expression
                | If LineInfo Expression Expression Expression
                | FunctionCall LineInfo String [Expression]
                deriving Show
                  

--TODO: make "If" a statement?
data Statement = Assignment LineInfo String Expression
               | Return LineInfo Expression
               deriving Show
                           
-- A function consists of a name, a list of agument variable names, and a
-- a list of statements for its body. The last statement must be a return
-- statement.
data Function = Function {
  getLineInfo :: LineInfo,
  getName :: String,
  getParams :: [String],
  getBody :: [Statement]
} deriving Show
                     

-- A program is simply a list of functions, one of which must be named
-- "main" 
newtype Program = Program [Function]
                  deriving Show