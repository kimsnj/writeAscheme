module Main where

import System.Environment
import Evaluator
import Parser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

