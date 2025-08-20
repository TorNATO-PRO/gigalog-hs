{-# LANGUAGE OverloadedStrings #-}
module Gigalog.Parser.Common where

import Text.Megaparsec (Parsec, MonadParsec (label), satisfy, many, manyTill, (<|>), between)
import Gigalog.Data.Common (Symbol(..))
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum, isLower, isUpper)
import qualified Data.Text as Text
import Text.Megaparsec.Char (char, space1)
import Data.Void (Void)
import Control.Monad (void)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

comma :: Parser Text
comma = symbol ","

period :: Parser ()
period = label "definition end delimiter" $ lexeme (void $ symbol ".")

ident :: Parser Text
ident = label "identifier" $
  L.lexeme sc $ do
    first <- satisfy (\c -> isAlphaNum c || c == '_')
    rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    pure $ Text.pack (first : rest)

lowerIdent :: Parser Text
lowerIdent = label "lower identifier" $
  L.lexeme sc $ do
    h <- satisfy (\c -> isLower c || c == '_')
    t <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    pure (Text.pack (h : t))

upperIdent :: Parser Text
upperIdent = label "upper identifier" $
  L.lexeme sc $ do
    h <- satisfy isUpper
    t <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    pure (Text.pack (h : t))

stringLiteral :: Parser Text
stringLiteral = label "String literal" $ L.lexeme sc $ Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

parseSymbol :: Parser Symbol
parseSymbol = label "Symbol" $ L.lexeme sc $ Symbol <$> (stringLiteral <|> lowerIdent)
