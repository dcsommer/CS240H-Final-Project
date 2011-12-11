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
  let program = parseMGDS toDebug programText
  either print (\prog -> print (run prog)) program
  return ()