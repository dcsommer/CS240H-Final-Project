{-# LANGUAGE OverloadedStrings #-}
module Parser(parseMGDS) where

import MGDS

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char

parseMGDS :: ByteString -> Program
parseMGDS path = Program []