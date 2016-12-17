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
import           Text.Parsec.Text     (Parser)

import qualified Data.Text            as T
import           Language.Ava.AST   as AST
import qualified Language.Ava.Lexer as Lexer

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
parseVector = Vector <$>  between (char '[') (char ']') p
    where p = Lexer.commaSep parseExpr

parseWord :: Parser AST.Value
parseWord = Word . T.unpack <$> Lexer.identifier

-- -- | Defines a parser for functions
-- --
-- -- Example
-- --
-- -- fn double (-- add numbers to the stack --) {
-- --   swap dup cons
-- -- }
-- parseProcedure :: Parser Value
-- parseProcedure = do
--       string "@define" <* spaces
--       p <- many1 alphaNum
--       lexeme $ parens (many $ noneOf ")")
--       body <- braces $ parseAST
--       return $ Procedure p body

parseExpr :: Parser AST.Value
parseExpr = parseNumber
        <|> parseString
        <|> parseVector
        <|> parseBoolean
        <|> parseWord

readExpr :: Parser a -> T.Text -> Either ParseError a
readExpr p = parse p "<stdin>"

parseMany :: T.Text -> Either ParseError [Value]
parseMany = readExpr (manyTill parseExpr eof)
