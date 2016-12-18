{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Parser
    ( parseInteger
    , parseFloat
    , parseBoolean
    , parseString
    , parseVector
    , parseWord
    , readExpr
    , parseMany
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
parseMany = readExpr (manyTill parseExpr eof)

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

parseVector :: Parser AST.Value
parseVector = Vector <$>  Lexer.brackets (Lexer.commaSep parseExpr)

parseWord :: Parser AST.Value
parseWord = Word . T.unpack <$> Lexer.identifier

parseProcedure :: Parser Value
parseProcedure = do
      try $ string "define" <* spaces
      -- The definition name
      p <- Lexer.identifier
      -- An optional comment
      optional $ Lexer.lexeme $ Lexer.parens (many $ noneOf ")")
      -- A list of expressions forming the definition body
      body <- Lexer.braces (many parseExpr)
      return $ Procedure (T.unpack p) body

parseExpr :: Parser AST.Value
parseExpr = parseNumber
        <|> parseProcedure
        <|> parseString
        <|> parseVector
        <|> parseBoolean
        <|> parseWord
