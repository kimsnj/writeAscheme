module Types where

import Data.IORef
import Data.Ratio
import Data.Complex
import Data.Maybe (isJust)

import System.IO (Handle)
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

-- AST of the implemented scheme
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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body   :: [LispVal]
                    , closure:: Env
                    }

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
showVal (PrimitiveFunc _)= "<primitive function>"
showVal (IOFunc _) = "<IO Primitive>"
showVal (Port _) = "<IO Port>"
showVal (Func args varargs _ _) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing  -> ""
     Just arg -> " . " ++ arg) ++ ") ...)"

showListVal :: [LispVal] -> String
showListVal = unwords . map showVal

instance Show LispVal where show = showVal

-- Type to support errors during parsing/interpretation
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (NumArgs expected found)      = "Expected " ++ show expected ++
                                          " args; found values " ++ unwordsList found
  where unwordsList = unwords . map show

instance Show LispError where show = showError

instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- LispError raised to the IO Monad
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT $ trapError action)

-- Type to hold REPL environment
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)
