{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Base.Parser
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Language.Ava.Base.Parser
    ( parseInteger
    , parseDouble
    , parseBoolean
    , parseString
    , parseList
    , parseWord
    , parseQuotation
    , parseMany
    , readExpr
    , AvaParseError
    ) where

import           Text.Parsec
import           Text.Parsec.Text   (Parser)

import Data.Bifunctor(bimap)

import qualified Data.Text          as T
import           Language.Ava.Base.AST   as AST
import qualified Language.Ava.Base.Lexer as Lexer

type AvaParseError = String

-------------------------------------------------------------

readExpr :: Parser a -> T.Text -> Either AvaParseError a
readExpr p input = bimap show id (parse p "<stdin>" input)

parseMany :: T.Text -> Either AvaParseError [AST.Value]
parseMany = readExpr $ manyTill parseExpr eof

-------------------------------------------------------------

parseInteger :: Parser AST.Prim
parseInteger = do
  digits <- Lexer.whiteSpace *> (many1 digit) <* Lexer.whiteSpace
  return $ AST.Integer (read digits)

parseDouble :: Parser AST.Prim
parseDouble = AST.Double <$> Lexer.float

parseNumber :: Parser AST.Prim
parseNumber = try parseDouble <|> parseInteger

parseBoolean :: Parser AST.Prim
parseBoolean = parseTrue <|> parseFalse
    where
      parseTrue = (Lexer.reserved "true") >> return (AST.Boolean True)
      parseFalse = (Lexer.reserved "false") >> return (AST.Boolean False)

parseString :: Parser AST.Prim
parseString = AST.String . T.unpack <$> Lexer.stringLiteral

parseList :: Parser AST.Prim
parseList = AST.List <$> Lexer.brackets (Lexer.commaSep parseExpr)

parseQuotation :: Parser AST.Prim
parseQuotation = (\xs -> AST.Quotation $ xs) <$> (Lexer.braces $ many parseExpr)

parseWord :: Parser AST.Prim
parseWord = AST.Word . T.unpack <$> Lexer.identifier

parsePrim :: Parser AST.Prim
parsePrim = parseNumber <|>
            parseQuotation <|>
            parseList <|>
            parseBoolean <|>
            parseWord

--------------------------------------------------
-- Control flow
--------------------------------------------------

parseDefine :: Parser AST.Value
parseDefine = do
    Lexer.reserved "define"
    name <- Lexer.identifier
    forms <- Lexer.braces (many parseExpr)
    return $ AST.Define (T.unpack name) forms

parseLet :: Parser AST.Value
parseLet = do
    Lexer.reserved "let"
    name <- Lexer.identifier
    Lexer.lexeme (char '=')
    expr <- parseExpr
    return $ AST.Let (T.unpack name) expr

parseExpr :: Parser AST.Value
parseExpr = (try parseLet <|> parseDefine) <|> (AST.Prim <$> parsePrim)
