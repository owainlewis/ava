{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Lexer
    ( lexer
    , identifier
    , reserved
    , operator
    , parens
    , brackets
    , braces
    , lexeme
    , integer
    , float
    , stringLiteral
    , commaSep
    , whiteSpace
    ) where

import           Data.Functor.Identity (Identity)
import           Text.Parsec
import           Text.Parsec.Text

import qualified Data.Text             as T
import qualified Text.Parsec.Language  as Lang
import qualified Text.Parsec.Token     as Token

-- |-----------------------------------------------------------------------

lexer :: Token.GenTokenParser T.Text st Identity
lexer = Token.makeTokenParser languageDef

readExpr :: Parser a -> T.Text -> Either ParseError a
readExpr p = parse p "<stdin>"

-- | -----------------------------------------------------------------------

languageDef :: Token.GenLanguageDef T.Text st Identity
languageDef = Lang.emptyDef {
    Token.commentStart = "{-"
  , Token.commentEnd = "-}"
  , Token.commentLine = "--"
  , Token.opStart = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = [ "if"
                          , "then"
                          , "else"
                          , "true"
                          , "false"
                          ]
  , Token.reservedOpNames = ["+"]
  , Token.caseSensitive = True
}

identifier :: Parser T.Text
identifier = T.pack <$> Token.identifier lexer

-- Parse a reserved name
reserved :: T.Text -> Parser ()
reserved op = Token.reserved lexer $ T.unpack op

operator :: T.Text -> Parser ()
operator op = Token.reservedOp lexer $ T.unpack op

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> Token.stringLiteral lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

whiteSpace :: Parser ()
whiteSpace  = Token.whiteSpace lexer
