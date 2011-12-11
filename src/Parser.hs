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
  where go  =  P.try ((:) <$> function <*> go)
           <|> space *> P.eof *> (pure [])

function :: Parser Function
function = space *>
           (Function <$> name <*> (parseTuple name) <*
                         matchStr "=" <*> expression)
           <* space

expression :: Parser Expression
expression  =  P.try ifParse
           <|> P.try comp
           <|> nested

ifParse :: Parser Expression
ifParse = If <$> (P.string "if " *> space *> expression)
             <*> (matchStr "then " *> expression)
             <*> (matchStr "else " *> expression)

functionCall :: Parser Expression
functionCall = FunctionCall <$> name <*> (parseTuple expression)

comp :: Parser Expression
comp  =  P.try (binop Equals "==" logic)
     <|> P.try (binop Less "<" logic)
     <|> P.try (binop Greater ">" logic)
     <|> logic

logic :: Parser Expression
logic  =  P.try (Not <$> (matchStr "!" *> factor))
      <|> P.try (binop LogicalAnd "&&" val)
      <|> P.try (binop LogicalOr "||" val)
      <|> val

val :: Parser Expression
val  =  P.try (binop Add "+" term)
    <|> P.try (binop Subtract "-" term)
    <|> term

term :: Parser Expression
term  =  P.try (binop Multiply "*" factor)
     <|> P.try (binop Divide "/" factor)
     <|> factor

factor :: Parser Expression
factor  =  nested
       <|> P.try ifParse
       <|> P.try functionCall
       <|> P.try (Var <$> name)
       <|> P.try (Constant <$> read <$> (many P.digit))

nested :: Parser Expression
nested = lparen *> expression <* rparen

-- Parser Helpers

name :: Parser String
name = (:) <$> P.letter <*> many P.alphaNum

parseTuple :: Parser a -> Parser [a]
parseTuple elemParse = 
  lparen *> list <* rparen
  where list  =  P.try ((:) <$> elemParse <*> list')
             <|> pure [] -- in the case of an empty tuple
        list' =  P.try ((:) <$> (comma *> elemParse) <*> list')
             <|> pure [] -- no more elements in the tuple

binop ctor op nxt = ctor <$> nxt <* matchStr op <*> nxt
line :: Parser Int
line =  do pos <- P.getPosition; return $ P.sourceLine pos
col :: Parser Int
col = do pos <- P.getPosition; return $ P.sourceColumn pos

--- Tokenizing
space   =  P.try (P.spaces *> comment *> space)
       <|> P.spaces

matchStr op = space *> P.string op *> space
comma   = matchStr ","
lparen  = P.string "(" *> space
rparen  = space *> P.string ")"
comment = P.string "//" *> many (P.noneOf "\n")