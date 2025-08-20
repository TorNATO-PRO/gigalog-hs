{-# LANGUAGE OverloadedStrings #-}
module Gigalog.Parser.Fact where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Gigalog.Syntax.AST (Fact (Fact))
import Gigalog.Parser.Common (parseSymbol, ident, parens, comma, period, sc)
import Gigalog.Data.Common (PredName(PredName))
import Text.Megaparsec (Parsec, sepBy1, manyTill, MonadParsec (eof, label), parse, errorBundlePretty)
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.Trans.Except (Except, throwE)
import Data.Set (Set)
import qualified Data.Set as Set

type Parser = Parsec Void Text

parseFact :: Parser Fact
parseFact = label "fact" $ do
  p <- PredName <$> ident
  args <- parens (parseSymbol `sepBy1` comma)
  period
  -- the invariant that the list is not empty is enforced
  -- by the parser
  pure $ Fact p $ NonEmpty.fromList args

factsFile :: Parser [Fact]
factsFile = sc *> manyTill parseFact eof

parseFactsText :: Text -> Text -> Except Text (Set Fact)
parseFactsText predName txt =
  case parse factsFile ("<" <> T.unpack predName <> ">") txt of
    Left e  -> throwE $ T.pack (errorBundlePretty e)
    Right f -> pure $ Set.fromList f
