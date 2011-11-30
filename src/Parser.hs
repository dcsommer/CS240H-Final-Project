{-# LANGUAGE OverloadedStrings #-}
module Parser(parseMGDS) where

import MGDS

import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char

parseMGDS :: ByteString -> Either String Program
parseMGDS path = P.parseOnly program path

program :: Parser Program
program = Program <$> go
  where go  =  ((:) <$> function <*> go)
           <|> (pure [])

function :: Parser Function
function = space *>  
           (Function <$> name <*> (parseTuple name) <*> expression)
           <* space


-- TODO FIXME: make "2+3" use the 'add' parser, not 'constant'
expression :: Parser Expression
expression = space *> (constant <|> var <|> add <|> subtractParse <|>
                       multiply <|> divide <|> equals <|> notParse <|>
                       logicalAnd <|> logicalOr <|> greater<|> less <|>
                       ifParse <|> functionCall) <* space

constant :: Parser Expression
constant = Constant <$> P8.decimal

var :: Parser Expression
var = Var <$> name

add :: Parser Expression
add = binop Add "+"

subtractParse :: Parser Expression
subtractParse = binop Subtract "-"

multiply :: Parser Expression
multiply = binop Multiply "*"

divide :: Parser Expression
divide = binop Divide "/"

equals :: Parser Expression
equals = binop Equals "=="

notParse :: Parser Expression
notParse = Not <$> (matchStr "!" *> expression)

logicalAnd :: Parser Expression
logicalAnd = binop LogicalAnd "&&"

logicalOr :: Parser Expression
logicalOr = binop LogicalOr "||"

greater :: Parser Expression
greater = binop Greater ">"

less :: Parser Expression
less = binop Less "<"

ifParse :: Parser Expression
ifParse = If <$> (matchStr "if" *> expression)
             <*> (matchStr "then" *> expression)
             <*> (matchStr "else" *> expression)

functionCall :: Parser Expression
functionCall = FunctionCall <$> name <*> (parseTuple expression)

-- Parser Helpers

name :: Parser String
name = B8.unpack <$> P8.takeWhile1 isLower

parseTuple :: Parser a -> Parser [a]
parseTuple elemParse = 
  lparen *> list <* rparen
  where list  =  (:) <$> elemParse <*> list'
                 <|> pure [] -- in the case of an empty tuple
        list' =  (:) <$> (comma *> elemParse) <*> list'
                 <|> pure [] -- no more elements in the tuple

binop ctor op = ctor <$> expression <* matchStr op <*> expression

--- Tokenizing
space   = P8.takeWhile isSpace
matchStr op = space *> string op *> space
comma   = matchStr ","
lparen  = matchStr "("
rparen  = matchStr ")"
comment = string "//" *> P.takeTill P8.isEndOfLine