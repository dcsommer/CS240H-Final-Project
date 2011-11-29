module Debugger where

import Parser
import Interpreter
import MGDS

import Control.Monad
import System.Environment
import System.IO
import qualified Data.ByteString as B

debugLoop :: Program -> IO ()
debugLoop program = do
  finished <- isEOF
  Control.Monad.unless finished $ do
    inputWords <- liftM words $ getLine
    -- handle inputWords, query the interpreter
    debugLoop program

main = do
  toDebug <- liftM head getArgs
  programText <- B.readFile toDebug
  debugLoop $ parseMGDS programText
  return ()