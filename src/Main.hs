module Main where

import Control.Monad
import System.IO
import System.Environment

import LispError
import Evaluator
import Parser

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint (head args)
            _ -> putStrLn "Program takes 0 or 1 argument"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> String
evalString expr = extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn $ evalString expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) $
    action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (=="quit") (readPrompt "> ") evalAndPrint
