module Debugger where

debugLoop :: Interpreter a => a -> IO ()
debugLoop interpreter = 
  finished <- isEOF
  Control.Monad.unless finished $ do
    inputLine <- getLine
    -- handle inputLine, query the interpreter
    debugLoop interpreter

main = do
  toDebug <- liftM head getArgs
  interpreter <- interpret toDebug
  debugLoop interpreter
  return ()
  -- loop on input