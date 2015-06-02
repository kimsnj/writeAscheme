module Parser where

import Numeric
import Data.Ratio
import Data.Complex
import Text.ParserCombinators.Parsec hiding (spaces)

-------------------
-- Token definition
-------------------
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
                 return [backslash, escaped]
                 

----------------
-- Token parsers
----------------
parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"") <|> escapedChar
                 _ <- char '"'
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


-- Simple types parsing
                                 
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

parseFloat :: Parser LispVal
parseFloat = do whole <- many1 digit
                _ <- char '.'
                rest <- many1 digit
                return $ Float $ read (whole ++ "." ++ rest)

parseComplex :: Parser LispVal
parseComplex = do real <- parseNumber <|> parseFloat
                  _ <- char '+'
                  imag <- parseNumber <|> parseFloat
                  _ <- char 'i'
                  return $ Complex (toDouble real :+ toDouble imag)
              where toDouble (Float d) = d
                    toDouble (Number n) = fromInteger n
                    toDouble n = error $ "Expected a number, received: " ++ show n

parseRational :: Parser LispVal
parseRational = do nom <- parseNumber
                   _ <- char '/'
                   den <- parseNumber
                   return $ Rational (toNum nom % toNum den)
                where toNum (Number n) = n
                      toNum n = error $ "Expected a number, received: " ++ show n

-- Recursive type parsing
parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExp spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExp spaces
                     tail <- char '.' >> spaces >> parseExp
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExp >>= (\e -> return $ List [Atom "quote", e])


parseExp :: Parser LispVal
parseExp = parseHashPrefix
           <|> parseAtom
           <|> parseString
           <|> try parseFloat
           <|> try parseComplex
           <|> try parseRational
           <|> parseNumber
           <|> parseQuoted
           <|> do _ <- char '('
                  x <- try parseList <|> try parseDottedList
                  _ <- char ')'
                  return x


readExpr :: String -> String
readExpr input = case parse parseExp "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
