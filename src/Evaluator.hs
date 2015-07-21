{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Types
import Data.IORef
import Data.Maybe (isNothing)

import Control.Monad.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "if", predicate, conseq, alt]) = if' env predicate conseq alt
eval env (List (Atom "cond" : clauses)) = cond env clauses
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

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
  `catchError` const (return False)

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
if' :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
if' env predicate conseq alt =
  do result <- eval env predicate
     case result of
       Bool False -> eval env alt
       Bool True  -> eval env conseq
       badArg     -> throwError $ TypeMismatch "bool" badArg

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env (List [Atom "else", conseq]:_) = eval env conseq
cond env (List [pred, conseq]:cls) =
  do result <- eval env pred
     case result of
       Bool True  -> eval env conseq
       Bool False -> cond env cls
       badArg     -> throwError $ TypeMismatch "bool" badArg
cond _ badArgList = throwError $ Default $ "Invalid `cond' statement: " ++ show badArgList

-- Mutable variables
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var= do env <- liftIO $ readIORef envRef
                      maybe (throwError $ UnboundVar "Getting an unbound var " var)
                            (liftIO . readIORef)
                            (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound var " var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBindings bindings)
        addBindings (var, value) = liftM ((,) var) (newIORef value)
