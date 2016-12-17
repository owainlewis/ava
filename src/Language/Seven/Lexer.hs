{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Seven.Lexer where

import Text.Parsec
import Text.Parsec.Text

import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T

import Language.Seven.AST
import Data.Functor.Identity (Identity)

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser languageDef

languageDef :: Token.GenLanguageDef T.Text () Identity
languageDef = Lang.emptyDef {
    Token.commentStart = "{-"
  , Token.commentEnd = "-}"
  , Token.commentLine = "--"
  , Token.opStart = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = []
  , Token.reservedOpNames = []
  , Token.caseSensitive = True
}

reservedOp :: String -> Parser ()
reservedOp op = Token.reservedOp lexer op

-- Parser surrounding parens
parens :: ParsecT T.Text () Identity a -> ParsecT T.Text () Identity a
parens = Token.parens lexer

-- Parse an integer
integer :: ParsecT T.Text () Identity Integer
integer = Token.integer lexer

-- Parser a floating point number
float :: ParsecT T.Text () Identity Double
float = Token.float lexer

-- Parser a string literal
stringLiteral :: ParsecT T.Text () Identity String
stringLiteral = Token.stringLiteral lexer

---------------------------------------

parseReserved :: Parser Value
parseReserved = (reservedOp "True" >> return (Boolean True))
            <|> (reservedOp "False" >> return (Boolean False))

languageParser :: Parser [Value]
languageParser = many parseReserved

--readExpr :: T.Text -> Either ParseError [Value]
readExpr p = parse p "<stdin>"
