module Parser where

import Numeric
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)


-------------------
-- Token definition
-------------------
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Show)

----------
-- Helpers
----------
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser String
escapedChar = do backslash <- char '\\'
                 escaped <- oneOf "nrt\"\\"
                 return $ [backslash, escaped]
                 

----------------
-- Token parsers
----------------
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"") <|> escapedChar
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom


parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) (many1 digit)

parseHashPrefix :: Parser LispVal
parseHashPrefix = char '#' >> (parseOctal
                               <|> parseDec
                               <|> parseHex
                               <|> parseBin
                               <|> parseChar
                               <|> parseBool)

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

parseChar :: Parser LispVal
parseChar = fmap Character (char '\\' >> anyChar)

parseBool :: Parser LispVal
parseBool = fmap (Bool . (== 't')) (oneOf "ft")

parseExp :: Parser LispVal
parseExp = parseHashPrefix
           <|> parseAtom
           <|> parseString
           <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExp "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ (show val)
