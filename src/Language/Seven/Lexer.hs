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
  , Token.opStart = Token.opLetter languageDef
  , Token.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedOpNames = ["+", "-", "*", "/", ":="]
}

identifier = Token.identifier lexer -- parses an identifier
reserved = Token.reserved lexer -- parses a reserved name
parens = Token.parens lexer -- parses surrounding parenthesis:

integer :: ParsecT T.Text () Identity Integer
integer = Token.integer lexer -- parses an integer

float :: ParsecT T.Text () Identity Double
float = Token.float lexer -- parses a double

whiteSpace = Token.whiteSpace lexer -- parses whitespace

reservedOp :: T.Text -> Parser ()
reservedOp op = Token.reservedOp lexer $ T.unpack op

parseReserved :: Parser Value
parseReserved =
              (reservedOp "True" >> return (Boolean True))
          <|> (reservedOp "False" >> return (Boolean False))

--readExpr :: T.Text -> Either ParseError Value
readExpr p = parse p "<stdin>"
