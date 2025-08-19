{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Gigalog.Parser (parseProgram)
import Gigalog.Validator (preprocess)
import Control.Monad.Trans.Except (runExcept, except)
import Data.Either.Combinators (mapLeft)
import Gigalog.Evaluator (evaluate, EvaluationEnv (EvaluationEnv, full))
import Gigalog.PrettyPrinter (displayTables)

main :: IO ()
main = do
  src <- T.getContents
  let
    result = runExcept do
      program <- except $ mapLeft (T.pack . errorBundlePretty) $ parse parseProgram "<stdin>" src
      validatedProgram <- preprocess program
      evaluate validatedProgram

  case result of
    Left e -> T.putStrLn e
    Right (EvaluationEnv { full }) -> do
      T.putStrLn $ displayTables full
