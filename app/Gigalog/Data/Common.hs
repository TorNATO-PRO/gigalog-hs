{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gigalog.Data.Common
  ( PredName (..),
    AttrName (..),
    VarName (..),
    Symbol (..),
    Row (..),
    Tables,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)

newtype PredName = PredName Text deriving (Eq, Ord, Show, Generic, NFData)

newtype AttrName = AttrName Text deriving (Eq, Ord, Show, Generic, NFData)

newtype VarName = VarName Text deriving (Eq, Ord, Show, Generic, NFData)

newtype Symbol = Symbol Text deriving (Eq, Ord, Show, Generic, NFData)

-- we can define an "entry" in our table as a non-empty list of symbols
newtype Row = Row (NonEmpty Symbol)
  deriving (Eq, Ord, Show, Generic, NFData)

-- and we define the tables as being a mapping of a predicate name to a set of rows
type Tables = Map.Map PredName (Set.Set Row)
