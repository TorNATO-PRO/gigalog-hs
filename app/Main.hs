{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
-- I don't care if they say it is deprecated. If they had
-- the balls they would remove the type entirely
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Monad.Trans.Except (ExceptT (..), withExceptT, Except, runExcept)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException (DecodeError, EncodeError))
import qualified Data.Text.IO as TIO
import Data.Traversable (for)
import Gigalog.CLI
import Gigalog.Data.Common (PredName (PredName))
import Gigalog.Parser.Fact (parseFactsText)
import Gigalog.Parser.Program (parseProgram)
import Gigalog.Syntax.AST (Program (Program), Fact)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest, setRequestHeaders)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Gigalog.Evaluator (EvaluationEnv(EvaluationEnv, full), evaluate)
import Gigalog.Validator (preprocess)
import qualified Data.Set as Set
import Data.Set (Set)
import Gigalog.PrettyPrinter (displayTables)

liftExcept :: Except e a -> ExceptT e IO a
liftExcept = ExceptT . pure . runExcept

loadProgram :: [(T.Text, T.Text)] -> Source -> ExceptT T.Text IO Program
loadProgram headers src = do
  txt <- fetchText headers src
  withExceptT (T.pack . errorBundlePretty) $
    ExceptT . pure $
      parse parseProgram (T.unpack $ displaySrc src) txt
  where
    displaySrc = \case
      Stdin -> "<stdin>"
      File fp -> "<" <> fp <> ">"
      Url dst -> "<" <> dst <> ">"

loadFacts ::  [(T.Text, T.Text)] -> [(PredName, Source)] -> ExceptT T.Text IO (Set Fact)
loadFacts headers factReferences =
  Set.unions
    <$> for
      factReferences
      ( \(PredName name, src) -> do
          txt <- fetchText headers src
          liftExcept $ parseFactsText name txt
      )

fetchText :: [(T.Text, T.Text)] -> Source -> ExceptT T.Text IO T.Text
fetchText headers src = ExceptT $
  case src of
    Stdin -> Right <$> TIO.getContents
    File fp -> do
      bs <- BS.readFile (T.unpack fp)
      pure $ first displayUnicodeException $ TE.decodeUtf8' bs
    Url dst -> do
      req0 <- parseRequest (T.unpack dst)
      let hdrs = [(CI.mk (B8.pack $ T.unpack k), B8.pack $ T.unpack v) | (k, v) <- headers]
          req = setRequestHeaders hdrs req0
      resp <- httpBS req
      let bs = getResponseBody resp
      pure $ first displayUnicodeException $ TE.decodeUtf8' bs

displayUnicodeException :: UnicodeException -> Text
displayUnicodeException (DecodeError err _) = T.pack err
displayUnicodeException (EncodeError err _) = T.pack err

main :: IO ()
main = do
  (Options {program, facts, headers}) <- parseCLI
  result <- runExceptT do
    Program programFacts rules <- loadProgram headers program
    f <- loadFacts headers facts 
    let prog = Program (Set.union f programFacts) rules

    validatedProgram <- liftExcept $ preprocess prog
    liftExcept $ evaluate validatedProgram

  case result of
    Left e -> TIO.putStrLn e
    Right (EvaluationEnv { full }) -> do
      TIO.putStrLn $ displayTables full
