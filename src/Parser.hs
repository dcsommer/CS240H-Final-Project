{-# LANGUAGE OverloadedStrings #-}
module Parser(parseMGDS) where

import MGDS

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import Control.Exception
import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char
import Data.Typeable

parseMGDS :: ByteString -> Either String Program
parseMGDS path = P.parseOnly program path

program :: Parser Program
program = P8.takeWhile isSpace *>
          (pure $ Program [])
          <* P8.takeWhile isSpace

function :: Parser Function
function = undefined

parseConstant :: Parser Expression
parseConstant = Constant <$> P8.decimal

parseVar :: Parser Expression
parseVar = Var <$> parseName

parseFunctionCall :: Parser Expression
parseFunctionCall = FunctionCall <$> parseName <*> parseCallArgs

parseCallArgs :: Parser [Expression]
parseCallArgs = string "(" *> 
                (pure []) 
                <* string ")"

parseName :: Parser String
parseName = B8.unpack <$> P8.takeWhile1 isLower