{-# LANGUAGE FlexibleContexts #-}
module Language.Seven.Parser
    ( parseSeven
    , parseAST
    , parseNumber
    , parseWord
    ) where

import           Language.Seven.AST
import           Text.Parsec
import           Text.Parsec.String (Parser)

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

parseBoolean :: Parser Value
parseBoolean = parseTrue <|> parseFalse
    where parseFalse = ulift (Boolean False) <$> string "false"
          parseTrue  = ulift (Boolean True) <$> string "true"
          ulift v = (\_ -> v)

-- | Generalized parser for wrapped structures like sexps etc
--
wrapped :: Char -> Parser a -> Char -> Parser a
wrapped l p r = let lexchar = lexeme . char in
                lexchar l *> p <* lexchar r

-- | Lifts a parser of a to a parser for { a }
--
braces :: Parser a -> Parser a
braces p = wrapped '{' p '}'

parens :: Parser a -> Parser a
parens p = wrapped '(' p ')'

-- | Defines a parser for functions
--
-- Example
--
-- fn double (-- add numbers to the stack --) {
--   swap dup cons
-- }
parseProcedure :: Parser Value
parseProcedure = do
      string "@define" <* spaces
      p <- many1 alphaNum
      lexeme $ parens (many $ noneOf ")")
      body <- braces $ parseAST
      return $ Procedure p body

parseWord :: Parser Value
parseWord = Word <$> many1 (symbol <|> alphaNum)

parseComment :: Parser Value
parseComment = do
  char '#'
  comment <- many $ noneOf "\n"
  return $ Comment comment

parseAST :: Parser [Value]
parseAST = many . lexeme $ astParser
    where astParser =
                parseProcedure
            <|> parseNumber
            <|> parseWord
            <|> parseComment

go :: Parser a -> String -> Either ParseError a
go p input = parse p ">>" input

parseSeven :: String -> Either ParseError [Value]
parseSeven input = go parseAST input
