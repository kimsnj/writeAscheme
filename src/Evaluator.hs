{-# LANGUAGE ExistentialQuantification #-}

module Evaluator (eval) where

import LispVal
import LispError

import Control.Monad.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) = if' predicate conseq alt
eval (List ((Atom "cond") : clauses)) = cond clauses
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("boolean?", oneArgFn isBoolean),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
             ]


-- Operators binary, unary and boolean
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _  []            = throwError $ NumArgs 2 []
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        =  mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [x, y] = do left  <- unpacker x
                                  right <- unpacker y
                                  return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

oneArgFn :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
oneArgFn fn [x] = return $ fn x
oneArgFn _  p   = throwError $ NumArgs 1 p

-- Unpackers from LispVal to "haskell" types
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = case reads s :: [(Integer, String)] of
    (x:_) -> return $ fst x
    _     -> throwError (TypeMismatch "number" (String s))
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _        = Bool False

-- Weak and strong equality
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return (unpacked1 == unpacked2)
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal l@[List _, List _] = return $ Bool $ listEqual l equal
equal [arg1, arg2] = do primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) unpackers
                        eqvEquals <- eqv [arg1, arg2]
                        return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
  where unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]

equal badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv l@[List _, List _]                 = return $ Bool $ listEqual l eqv
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

listEqual :: [LispVal] -> ([LispVal] -> ThrowsError LispVal) ->  Bool
listEqual [List arg1, List arg2] op = length arg1 == length arg2
                                       && all pairEqual (zip arg1 arg2)
     where pairEqual (x1, x2) = case op [x1, x2] of
                                Left _           -> False
                                Right (Bool val) -> val

-- List operations
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]          = return x
car [DottedList (x:_) _]  = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return (List xs)
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return (DottedList xs x)
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return (List [x])
cons [x, List xs] = return (List (x:xs))
cons [x, DottedList xs xlast] = return (DottedList (x:xs) xlast)
cons [x1, x2] = return (DottedList [x1] x2)
cons badArgList = throwError $ NumArgs 2 badArgList

-- Conditionals
if' :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
if' predicate conseq alt =
  do result <- eval predicate
     case result of
       Bool False -> eval alt
       Bool True  -> eval conseq
       badArg     -> throwError $ TypeMismatch "bool" badArg

cond :: [LispVal] -> ThrowsError LispVal
cond ((List [Atom "else", conseq]):_) = eval conseq
cond ((List [pred, conseq]):cls) =
  do result <- eval pred
     case result of
       Bool True  -> eval conseq
       Bool False -> cond cls
       badArg     -> throwError $ TypeMismatch "bool" badArg
cond badArgList = throwError $ Default "Invalid `cond' statement.`"
