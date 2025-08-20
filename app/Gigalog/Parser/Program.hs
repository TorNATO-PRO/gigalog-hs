{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gigalog.Parser.Program (parseProgram) where

import Control.Monad (void)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Gigalog.Data.Common
  ( PredName (PredName),
    VarName (VarName),
  )
import Gigalog.Syntax.AST
  ( Atom (Atom),
    Program (Program),
    Rule (Rule, ruleHead, ruleBody),
    Term (TSym, TVar),
    Fact (..)
  )
import Text.Megaparsec
  ( MonadParsec (eof, label, try),
    choice,
    manyTill,
    sepBy1,
    (<?>),
    (<|>),
  )
import Gigalog.Parser.Common (upperIdent, parseSymbol, lowerIdent, ident, Parser, lexeme, symbol, parens, comma, period, sc)
import Gigalog.Parser.Fact (parseFact)
import qualified Data.Set as Set

-- here are some various built-ins to the language that we must parse

implies :: Parser ()
implies = lexeme (void $ symbol ":-") <?> "rule implication"

questionMark :: Parser Text
questionMark = lexeme (symbol "?") <?> "variable definition start"

-- now we can begin defining parsers for the AST

parseVariable :: Parser Term
parseVariable =
  label "variable" $
    choice
      [ questionMark >> (TVar . VarName <$> (lowerIdent <|> upperIdent)),
        TVar . VarName <$> upperIdent
      ]

parseTerm :: Parser Term
parseTerm = label "term" $ lexeme $ do
  -- a term is either a variable or a string
  try parseVariable <|> (TSym <$> parseSymbol)

parseAtom :: Parser Atom
parseAtom = label "Atom" $ do
  -- we grab the identifier
  identifier <- PredName <$> ident

  -- and we grab the terms that are present
  terms <- NonEmpty.fromList <$> parens (sepBy1 parseTerm comma)

  -- we know that terms MUST be nonempty
  pure $ Atom identifier terms

parseRule :: Parser Rule
parseRule = label "rule" $ do
  -- we grab the rule head
  h <- parseAtom

  -- we get rid of the reverse implication by eating it
  implies

  -- obtain the rule body - there must be at least one rule in the rule body
  body <- sepBy1 parseAtom comma

  -- all rule bodies must end with a period
  period

  pure $ Rule { ruleHead = h, ruleBody = NonEmpty.fromList body }

parseFactOrRule :: Parser (Either Rule Fact)
parseFactOrRule = (Left <$> try parseRule) <|> (Right <$> parseFact)

parseProgram :: Parser Program
parseProgram = do
  sc
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
  pure $ Program (Set.fromList facts) (Set.fromList rules)
