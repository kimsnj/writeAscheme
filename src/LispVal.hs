module LispVal  ( LispVal(..)  ) where

import Data.Ratio
import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Complex (Complex Double)
             | Rational Rational
             | String String
             | Bool Bool
             | Character Char

showVal :: LispVal -> String
showVal (Atom s)         = s
showVal (List l)         = "(" ++ showListVal l ++ ")"
showVal (DottedList h t) = "(" ++ showListVal h ++ " . " ++  showVal t ++ ")"
showVal (Number i)       = show i
showVal (Float d)        = show d
showVal (Rational r)     = show (numerator r) ++ "/" ++ show (denominator r)
showVal (String s)       = "\"" ++ s ++ "\""
showVal (Bool True)      = "#t"
showVal (Bool False)     = "#f"
showVal (Character c)    = [c]
showVal (Complex (real :+ img)) = show real ++ " + " ++ show img ++ "i"

showListVal :: [LispVal] -> String
showListVal = unwords . map showVal

instance Show LispVal where show = showVal

