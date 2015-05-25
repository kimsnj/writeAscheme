module Main where

import Numeric
import System.Environment
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser String
escapedChar = do backslash <- char '\\'
                 escaped <- oneOf "nrt\"\\"
                 return $ [backslash, escaped]
                 

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"") <|> escapedChar
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parsePlainNumber :: Parser LispVal
parsePlainNumber = fmap (Number . read) (many1 digit)

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> (parseOctal <|> parseDec <|> parseHex <|> parseBin)

parseNumberHelper prefix filter reader = do char prefix
                                            number <- many1 filter
                                            return $ (Number . reader) number
                                     
parseOctal :: Parser LispVal
parseOctal = parseNumberHelper 'o' octDigit (fst . head . readOct)

parseHex :: Parser LispVal
parseHex = parseNumberHelper 'h' hexDigit (fst . head . readHex)

parseDec :: Parser LispVal
parseDec = parseNumberHelper 'd' digit read

parseBin :: Parser LispVal
parseBin = parseNumberHelper 'b' (oneOf "01") binToDec
  where binToDec = foldl (\acc x -> acc * 2 + (if x == '0' then 0 else 1)) 0

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber


parseExp :: Parser LispVal
parseExp = parseNumber <|> parseAtom <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExp "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ (show val)


