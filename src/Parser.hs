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
parseMGDS text = P.parseOnly program text
                 --TODO give empty bytestring to indicate end of input

program :: Parser Program
program = Program <$> go
  where go  =  ((:) <$> function <*> go)
           <|> space *> comment *> go
           <|> space *> P.endOfInput *> (pure [])

function :: Parser Function
function = space *>  
           (Function <$> name <*> (parseTuple name) <*
                         matchStr "=" <*> expression)
           <* space

expression :: Parser Expression
expression  =  ifParse 
           <|> functionCall
           <|> comp
           <|> nested

ifParse :: Parser Expression
ifParse = If <$> (string "if " *> space *> expression)
             <*> (matchStr "then " *> expression)
             <*> (matchStr "else " *> expression)

functionCall :: Parser Expression
functionCall = FunctionCall <$> name <*> (parseTuple expression)

comp :: Parser Expression
comp  =  binop Equals "==" logic
     <|> binop Less "<" logic
     <|> binop Greater ">" logic
     <|> logic

logic :: Parser Expression
logic  =  binop LogicalAnd "&&" val
      <|> binop LogicalOr "||" val
      <|> Not <$> (matchStr "!" *> factor)
      <|> val

val :: Parser Expression
val  =  binop Add "+" term
    <|> binop Subtract "-" term
    <|> term

term :: Parser Expression
term  =  binop Multiply "*" factor
     <|> binop Divide "/" factor
     <|> factor

factor :: Parser Expression
factor  =  Constant <$> P8.decimal
       <|> Var <$> name
       <|> nested

nested :: Parser Expression
nested = lparen *> expression <* rparen

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

binop ctor op nxt = ctor <$> nxt <* matchStr op <*> nxt

--- Tokenizing
space   = P8.takeWhile isSpace
matchStr op = space *> string op *> space
comma   = matchStr ","
lparen  = string "(" *> space
rparen  = space *> string ")"
comment = string "//" *> P.takeTill P8.isEndOfLine