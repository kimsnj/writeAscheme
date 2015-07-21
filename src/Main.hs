module Main where

import Control.Monad

import System.IO
import System.Environment

import Types
import Evaluator
import Parser

-- The REPL loop
main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOne (head args)
            _ -> putStrLn "Program takes 0 or 1 argument"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) $
    action result >> until_ predicate prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (=="quit") (readPrompt "> ") . evalAndPrint

