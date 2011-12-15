import Parser
import Interpreter
import MGDS
import Hindsight

import Data.Foldable
import Data.Map
import Control.Monad
import System.Environment
import System.IO
import System.Exit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Debug.Trace

(+++) a b = a ++ ('\n':b)

helpHint = "Type 'help' for command list."

showHelp = putStrLn $
     "Command List:"
 +++ "help         : displays help information"
 +++ "run          : prints the return value of the program"
 +++ "s <line>     : prints the state of the stack frame of the enclosing"
 +++ "               function at that line of code" 
 +++ "fs <line>    : prints the state of the entire stack (backtrace) at"
 +++ "               the specified line of code"
 +++ "p            : prints a representation of the AST"
 +++ "exit         : exit the debugger"
       
unknown = putStrLn $ "Unknown command. " ++ helpHint

boundLine m l = (max (l-4) 1, min m (l+4))

printLines :: Map Int B.ByteString -> Int -> IO ()
printLines ls at = printLines' low where
  (low,high) = boundLine (size ls) at
  printLines' n | n > high = return ()
                | n <= high = do
    putStr $ show n
    putStr ":\t"
    B.putStrLn $ ls ! n
    printLines' $ n+1

debugLoop :: B.ByteString -> Program -> IO ()
debugLoop txt program = do
  putStrLn $ "Welcome to the MGDS Hindsight Debugger. " ++ helpHint
  let lineMap = foldl' (\m l -> insert (1 + size m) l m) empty $ B8.lines txt
  debugLoop2 lineMap program

debugLoop2 :: Map Int B.ByteString -> Program -> IO ()
debugLoop2 lineMap program@(Program fenv) = do
  let (ret, map) = run program
  putStr "> "
  hFlush stdout
  finished <- isEOF
  Control.Monad.unless finished $ do
    inputWords <- liftM words $ getLine
    case inputWords of
      (cmd:args) -> case cmd of
        "help"  -> showHelp
        "run"   -> putStrLn $ show ret
        "r"     -> putStrLn $ show ret
        "s"     -> showShortState map (read $ head args) fenv
        "bt"    -> showFullState  map (read $ head args) fenv
        "fs"    -> showFullState  map (read $ head args) fenv
        "p"     -> putStrLn $ show program
        "l"     -> printLines lineMap $ read $ head args
        "exit"  -> exitSuccess
        _       -> unknown
      _          -> unknown
    debugLoop2 lineMap program

main = do
  args <- getArgs
  if length args < 1
    then putStrLn "Usage: ./Debugger <program.mgds>"
    else do toDebug <- liftM head getArgs
            programText <- B.readFile toDebug
            let program = parseMGDS toDebug programText
            either putStrLn (debugLoop programText) program
            return ()