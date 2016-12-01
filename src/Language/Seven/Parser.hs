{-# LANGUAGE FlexibleContexts #-}
module Language.Seven.Parser
    ( go
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

parseNumber :: Parser Value
parseNumber = Number <$> integer

-- proc double (-- add numbers to the stack --) =>
--   swap dup cons
--   flip reverse @@>
-- end
parseProcedure :: Parser Value
parseProcedure = do
      string "proc"
      spaces
      p <- many1 alphaNum
      lexeme $ string "=>"
      body <- many1 $ parseNumber <|> parseWord
      spaces *> string "end"
      return $ Procedure p body

-- | Parse a word. For now use alphanum but needs to support more chars
--
parseWord :: Parser Value
parseWord = Word <$> many1 alphaNum

parseAST :: Parser [Value]
parseAST = many1 . lexeme $ astParser
    where astParser = parseNumber <|> parseWord

go :: Parser a -> String -> Either ParseError a
go p input = parse p ">>" input
