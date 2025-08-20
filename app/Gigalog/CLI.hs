{-# LANGUAGE OverloadedStrings #-}

module Gigalog.CLI where

import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Text as T
import Gigalog.Data.Common (PredName (..))
import Options.Applicative
  ( Alternative (many),
    Parser,
    customExecParser,
    eitherReader,
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    showHelpOnEmpty,
    showHelpOnError,
  )

data Source
  = Stdin
  | File Text
  | Url Text
  deriving (Show, Eq)

data Options = Options
  { facts :: [(PredName, Source)],
    program :: Source,
    headers :: [(Text, Text)]
  }
  deriving (Eq, Show)

parseCLI :: IO Options
parseCLI =
  customExecParser
    (prefs (showHelpOnError <> showHelpOnEmpty))
    ( info
        (helper <*> versioner <*> opts)
        ( fullDesc
            <> progDesc "Gigalog: a seminaive Datalog engine implementing parallel joins"
            <> header "gigalog - a elegant semi-naive datalog"
        )
    )
  where
    versioner =
      infoOption
        "gigalog 0.1.0"
        (long "version" <> help "Show version")

    opts :: Parser Options
    opts =
      Options
        <$> many factP
        <*> programP
        <*> many headerP

    factP :: Parser (PredName, Source)
    factP =
      option
        (eitherReader readPredSource)
        ( long "facts"
            <> metavar "PRED:SRC"
            <> help "Facts for a predicate from file/url/stdin; e.g. edge:file:edges.csv | path:url:https://.. | color:stdin | edge[seed]:file:edges.csv"
        )

    readPredSource :: String -> Either String (PredName, Source)
    readPredSource s = do
      (pred', rest) <- splitOnce ':' s
      src <- readSource rest
      pure (PredName (T.pack pred'), src)

    programP :: Parser Source
    programP =
      option
        (eitherReader readSource)
        ( long "program"
            <> metavar "SRC"
            <> help ("program" <> " source: file:PATH | url:URL | stdin")
        )

    headerP :: Parser (Text, Text)
    headerP =
      option
        (eitherReader readHeaderKV)
        ( long "header"
            <> metavar "\"Key: Value\""
            <> help "HTTP header for URL sources (repeatable)"
        )

    readSource :: String -> Either String Source
    readSource s
      | s == "stdin" = Right Stdin
      | Just rest <- stripPrefix "file:" s = Right (File $ T.pack rest)
      | Just rest <- stripPrefix "url:" s = Right (Url $ T.pack rest)
      | otherwise = Left "Use file:PATH | url:URL | stdin"

    readHeaderKV :: String -> Either String (Text, Text)
    readHeaderKV raw =
      case break (== ':') raw of
        (k, ':' : v) ->
          let k' = T.strip (T.pack k)
              v' = T.strip (T.pack v)
           in if T.null k'
                then Left "Header key cannot be empty"
                else Right (normalizeHeaderKey k', v')
        _ -> Left "Expected \"Key: Value\""

    normalizeHeaderKey :: Text -> Text
    normalizeHeaderKey =
      T.intercalate "-" . map cap . T.splitOn "-"
      where
        cap t = case T.uncons (T.toLower t) of
          Nothing -> t
          Just (c, r) -> T.cons (C.toUpper c) r

    stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
    stripPrefix p xs = case splitAt (length p) xs of
      (p', ys) | p' == p -> Just ys
      _ -> Nothing

    splitOnce :: Char -> String -> Either String (String, String)
    splitOnce c xs =
      case break (== c) xs of
        (a, _ : b) | not (null a) && not (null b) -> Right (a, b)
        _ -> Left ("Expected '<pred>:<src>', got: " <> xs)
