{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Parser
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Language.Ava.Parser
    ( parseInteger
    , parseFloat
    , parseBoolean
    , parseString
    , parseList
    , parseWord
    , parseQuotation
    , parseMany
    , readExpr
    ) where

import           Text.Parsec
import           Text.Parsec.Text   (Parser)

import qualified Data.Text          as T
import           Language.Ava.AST   as AST
import qualified Language.Ava.Lexer as Lexer

-------------------------------------------------------------

readExpr :: Parser a -> T.Text -> Either ParseError a
readExpr p = parse p "<stdin>"

parseMany :: T.Text -> Either ParseError [Value]
parseMany = readExpr $ manyTill parseExpr eof

-------------------------------------------------------------

parseInteger :: Parser AST.Value
parseInteger = AST.Integer . fromIntegral <$> Lexer.integer

parseFloat :: Parser AST.Value
parseFloat = AST.Float <$> Lexer.float

parseNumber :: Parser AST.Value
parseNumber = try parseFloat <|> parseInteger

parseBoolean :: Parser AST.Value
parseBoolean = parseTrue <|> parseFalse
  where
    parseTrue =
      (Lexer.reserved "true") >> (return $ AST.Boolean True)
    parseFalse =
      (Lexer.reserved "false") >> (return $ AST.Boolean False)

parseString :: Parser AST.Value
parseString = AST.String . T.unpack <$> Lexer.stringLiteral

parseList :: Parser AST.Value
parseList = List <$>  Lexer.brackets (Lexer.commaSep parseExpr)

parseQuotation :: Parser AST.Value
parseQuotation = (\xs -> Quotation xs) <$> (Lexer.braces $ many parseExpr)

parseWord :: Parser AST.Value
parseWord = Word . T.unpack <$> Lexer.identifier

-- define square =
--   dup *
--  ;
parseDefine :: Parser AST.Value
parseDefine = do
  Lexer.reserved "define"
  name <- Lexer.identifier
  char '='
  forms <- Lexer.braces $ spaces *> (many parseExpr) <* spaces
  return $ AST.Define (T.unpack name) forms

parseLet :: Parser AST.Value
parseLet = do
  Lexer.reserved "let"
  name <- Lexer.identifier
  char '='
  expr <- parseExpr
  return $ AST.Let (T.unpack name) expr

parseExpr :: Parser AST.Value
parseExpr = try parseNumber
        <|> parseDefine
        <|> parseLet
        <|> parseQuotation
        <|> parseString
        <|> parseList
        <|> parseBoolean
        <|> parseWord
