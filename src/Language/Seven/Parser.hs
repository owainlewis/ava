{-# LANGUAGE FlexibleContexts #-}
module Language.Seven.Parser
    ( parseSeven
    , parseAST
    , parseNumber
    , parseWord
    ) where

import Language.Seven.AST
import Text.Parsec
import Text.Parsec.String(Parser)

float :: Parser Float
float = fmap read $ (many1 digit) <++> decimal
    where
      decimal = option "" $ char '.' <:> (many1 digit)
      (<++>) a b = (++) <$> a <*> b
      (<:>) a b = (:) <$> a <*> b

integer :: Parser Int
integer = read <$> (positiveInteger <|> negativeInteger <|> integer)
    where integer = many1 digit
          positiveInteger = char '+' *> integer
          negativeInteger = char '-' <:> integer
          (<:>) a b = (:) <$> a <*> b

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

symbol :: Parser Char
symbol = oneOf "+-*"

parseNumber :: Parser Value
parseNumber = Number <$> integer

-- | Lifts a parser of a to a parser for { a }
--
braces :: Parser a -> Parser a
braces p = lexchar '{' *> p <* lexchar '}'
    where lexchar = lexeme . char

-- | Defines a parser for functions
--
-- Example
--
-- fn double (-- add numbers to the stack --) {
--   swap dup cons
-- }
parseProcedure :: Parser Value
parseProcedure = do
      string "fn" <* spaces
      p <- many1 alphaNum
      body <- braces $ parseAST
      return $ Procedure p body

parseWord :: Parser Value
parseWord = Word <$> many1 (symbol <|> alphaNum)

parseAST :: Parser [Value]
parseAST = many1 . lexeme $ astParser
    where astParser =
                try parseProcedure
            <|> parseNumber
            <|> parseWord

go :: Parser a -> String -> Either ParseError a
go p input = parse p ">>" input

parseSeven :: String -> Either ParseError [Value]
parseSeven input = go parseAST input
