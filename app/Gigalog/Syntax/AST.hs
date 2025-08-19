module Gigalog.Syntax.AST (Term(..), Atom(..), Rule(..), Fact(..), Program(..)) where

import Gigalog.Data.Common (PredName, Symbol, VarName)
import Data.List.NonEmpty (NonEmpty)

data Term = TVar VarName | TSym Symbol
  deriving (Eq, Ord, Show)

data Atom = Atom PredName (NonEmpty Term)
  deriving (Eq, Ord, Show)

data Rule = Rule
  { ruleHead :: Atom
  , ruleBody :: NonEmpty Atom
  }
  deriving (Eq, Ord, Show)

data Fact = Fact PredName (NonEmpty Symbol)
  deriving (Eq, Ord, Show)

data Program = Program
  { facts :: [Fact]
  , rules :: [Rule]
  }
  deriving (Eq, Show)
