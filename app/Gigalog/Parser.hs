{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gigalog.Parser (parseProgram) where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Gigalog.Data.Common (PredName (PredName), Symbol (Symbol), VarName (VarName))
import Gigalog.Syntax.AST (Atom (Atom), Fact (Fact), Program (Program), Rule (Rule), Term (TSym, TVar))
import Text.Megaparsec
  ( MonadParsec (eof, label, try),
    Parsec,
    ShowErrorComponent (showErrorComponent),
    between,
    choice,
    customFailure,
    many,
    manyTill,
    satisfy,
    sepBy1,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec CustomError Text

data CustomError
  = EmptyArgList PredName
  | EmptyRuleBody PredName
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomError where
  showErrorComponent :: CustomError -> String
  showErrorComponent err = case err of
    (EmptyArgList (PredName p)) -> T.unpack $ "empty argument list for predicate '" <> p <> "' (expected at least one term)"
    (EmptyRuleBody (PredName p)) -> T.unpack $ "empty rule body for predicate '" <> p <> "' (expected at least one rule)"

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

-- here are some various built-ins to the language that we must parse

implies :: Parser ()
implies = lexeme (void $ symbol ":-") <?> "rule implication"

period :: Parser ()
period = lexeme (void $ symbol ".") <?> "definition end delimiter"

questionMark :: Parser Text
questionMark = lexeme (symbol "?") <?> "variable definition start"

-- now we can begin defining parsers for the AST

ident :: Parser Text
ident = label "identifier" $
  lexeme $ do
    first <- satisfy (\c -> isAlphaNum c || c == '_')
    rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    pure $ Text.pack (first:rest)

stringLiteral :: Parser Text
stringLiteral = label "String literal" $ lexeme $ Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

parseTerm :: Parser Term
parseTerm = label "term" $ lexeme $ do
  -- a term is either a variable or a string
  try parseVariable <|> (TSym <$> parseSymbol)

parseVariable :: Parser Term
parseVariable =
  label "variable" $
    choice
      [ questionMark >> (\i -> TVar $ VarName ("?" <> i)) <$> ident,
        TVar . VarName <$> ident
      ]

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between (symbol "(") (symbol ")")

parseSymbol :: Parser Symbol
parseSymbol = label "Symbol" $ lexeme $ Symbol <$> (stringLiteral <|> ident)

parseAtom :: Parser Atom
parseAtom = label "Atom" $ do
  -- we grab the identifier
  identifier <- PredName <$> ident

  -- and we grab the terms that are present
  terms <- betweenParentheses (sepBy1 parseTerm (symbol ","))

  -- we attempt to parse the terms list into a non-empty list
  case NonEmpty.nonEmpty terms of
    Just t -> pure $ Atom identifier t
    Nothing -> customFailure $ EmptyArgList identifier

parseRule :: Parser Rule
parseRule = label "rule" $ do
  -- we grab the rule head
  ruleHead@(Atom predName _) <- parseAtom

  -- we get rid of the reverse implication by eating it
  implies

  -- obtain the rule body - there must be at least one rule in the rule body
  ruleBody <- sepBy1 parseAtom (symbol ",")

  -- all rule bodies must end with a period
  period

  -- we attempt to parse the body into a non-empty list
  case NonEmpty.nonEmpty ruleBody of
    Just body -> pure $ Rule ruleHead body
    Nothing -> customFailure (EmptyRuleBody predName)

parseFact :: Parser Fact
parseFact = label "Fact" $ do
  -- we grab the fact identfier
  factName <- PredName <$> ident

  -- and we grab the terms that are present
  factBody <- betweenParentheses (sepBy1 parseSymbol (symbol ","))

  -- all facts must end with a period
  period

  -- we attempt to parse the terms list into a non-empty list
  case NonEmpty.nonEmpty factBody of
    Just body -> pure $ Fact factName body
    Nothing -> customFailure (EmptyArgList factName)

parseFactOrRule :: Parser (Either Rule Fact)
parseFactOrRule = (Left <$> try parseRule) <|> (Right <$> parseFact)

parseProgram :: Parser Program
parseProgram = do
  factsOrRules <- manyTill parseFactOrRule eof

  -- extract all the facts
  let facts =
        mapMaybe
          ( \case
              Left _ -> Nothing
              Right fact -> Just fact
          )
          factsOrRules

  -- extract all the rules
  let rules =
        mapMaybe
          ( \case
              Left rule -> Just rule
              Right _ -> Nothing
          )
          factsOrRules

  -- profit
  pure $ Program facts rules
