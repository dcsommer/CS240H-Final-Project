module MGDS where

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
                  deriving Show
                  
-- A function consists of a name, a list of agument variable names, and an
-- expression for its body
data Function = Function String [String] Expression

-- A program is simply a list of functions, one of which must be named
-- "main" 
newtype Program = Program [Function]