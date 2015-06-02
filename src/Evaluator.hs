module Evaluator (eval) where

import Parser (LispVal(..))

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (map eval args)
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", oneArgFn isBoolean)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op = Number . foldl1 op . map unpackNum

oneArgFn :: (LispVal -> LispVal) -> [LispVal] -> LispVal
oneArgFn fn [x] = fn x
oneArgFn _ _ = error "Expected only one argument."

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = case reads s :: [(Integer, String)] of
    (x:_) -> fst x
    _     -> 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _        = Bool False