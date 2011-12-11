import Parser
import Interpreter
import MGDS

import Control.Monad
import System.Environment
import System.IO
import qualified Data.ByteString as B

(+++) a b = a ++ ('\n':b)

helpHint = "Type 'help' for command list."

showHelp = putStrLn $
           "Command List:"
       +++ "help         : displays help information"
       +++ "run          : runs the program and echos its output"
       +++ "p            : prints a representation of the AST"
       
unknown = putStrLn $ "Unknown command. " ++ helpHint

debugLoop :: Program -> IO ()
debugLoop program = do
  putStrLn $ "Welcome to the MGDS Historical Debugger. " ++ helpHint
  debugLoop2 program
  
debugLoop2 program = do
  putStr "> "
  hFlush stdout
  finished <- isEOF
  Control.Monad.unless finished $ do
    inputWords <- liftM words $ getLine
    case inputWords of
      (cmd:args) -> case cmd of
        "help"  -> showHelp
        "run"   -> putStrLn $ show $ run program
        "p"     -> putStrLn $ show program
        _       -> unknown
      _          -> unknown
    debugLoop2 program

main = do
  args <- getArgs
  if length args < 1
    then putStrLn "Usage: ./Debugger <program.mgds>"
    else do toDebug <- liftM head getArgs
            programText <- B.readFile toDebug
            let program = parseMGDS toDebug programText
            either putStrLn debugLoop program
            return ()