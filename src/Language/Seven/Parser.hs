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

-- fn double (-- add numbers to the stack --) =>
--   swap dup cons
--   flip reverse @@>
-- ;
parseProcedure :: Parser Value
parseProcedure = do
      string "@" <* spaces
      p <- many1 alphaNum
      lexeme $ string "=>"
      body <- parseAST
      spaces *> string ";"
      return $ Procedure p body

parseWord :: Parser Value
parseWord = Word <$> many1 (symbol <|> alphaNum)

parseAST :: Parser [Value]
parseAST = many1 . lexeme $ astParser
    where astParser =
                parseProcedure
            <|> parseNumber
            <|> parseWord

go :: Parser a -> String -> Either ParseError a
go p input = parse p ">>" input

parseSeven :: String -> Either ParseError [Value]
parseSeven input = go parseAST input
