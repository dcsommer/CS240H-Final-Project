{-# LANGUAGE OverloadedStrings #-}
module Parser(parseMGDS) where

import MGDS

import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Data.Word (Word8)
import Data.Char

---- Parsec
import qualified Text.Parsec as P
import Text.Parsec.ByteString
--import Text.Parsec.Token
import Text.Parsec.Prim
---- End Parsec

parseMGDS :: String -> ByteString -> Either String Program
parseMGDS fname input = either (Left . show) (Right . id) $
                          runParser program () fname input

program :: Parser Program
program = Program <$> go
  where go  =  ((:) <$> function <*> go)
           <|> space *> P.eof *> (pure [])

function :: Parser Function
function = space *>  
           (Function <$> name <*> (parseTuple name) <*
                         matchStr "=" <*> expression)
           <* space

expression :: Parser Expression
expression  =  ifParse 
           <|> comp
           <|> nested

ifParse :: Parser Expression
ifParse = If <$> (P.string "if " *> space *> expression)
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
logic  =  Not <$> (matchStr "!" *> factor)
      <|> binop LogicalAnd "&&" val
      <|> binop LogicalOr "||" val
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
factor  =  nested
       <|> ifParse
       <|> functionCall
       <|> Constant <$> read <$> (many P.digit)
       <|> Var <$> name

nested :: Parser Expression
nested = lparen *> expression <* rparen

-- Parser Helpers

name :: Parser String
name = B8.unpack <$> P.letter <*> (many P.alphaNum)

parseTuple :: Parser a -> Parser [a]
parseTuple elemParse = 
  lparen *> list <* rparen
  where list  =  (:) <$> elemParse <*> list'
                 <|> pure [] -- in the case of an empty tuple
        list' =  (:) <$> (comma *> elemParse) <*> list'
                 <|> pure [] -- no more elements in the tuple

binop ctor op nxt = ctor <$> nxt <* matchStr op <*> nxt
line = P.sourceLine . P.getPosition
col = P.sourceColumn . P.getPosition

--- Tokenizing
space   =  (P.spaces *> comment *> P.spaces) <|> P.spaces

matchStr op = space *> P.string op *> space
comma   = matchStr ","
lparen  = P.string "(" *> space
rparen  = space *> P.string ")"
comment = P.string "//" *> many (P.noneOf "\n")