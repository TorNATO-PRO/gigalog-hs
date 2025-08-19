{-# LANGUAGE OverloadedStrings #-}

module Gigalog.PrettyPrinter (displayTables) where

import Data.List (transpose)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Gigalog.Data.Common
  ( PredName (PredName),
    Row (Row),
    Symbol (Symbol),
    Tables,
  )

displayTables :: Tables -> T.Text
displayTables tables =
  T.intercalate "\n\n" $
    map renderPred (Map.toAscList tables)
  where
    renderPred :: (PredName, Set.Set Row) -> T.Text
    renderPred (PredName p, rows)
      | Set.null rows = p <> " (empty)"
      | otherwise =
          let cells :: [[T.Text]]
              cells = map rowToCells (Set.toAscList rows)
              arity = maybe 0 length (safeHead cells)
              header = p <> "/" <> T.pack (show arity) <> " (" <> T.pack (show (Set.size rows)) <> " rows)"
           in header <> "\n" <> boxTable cells

    rowToCells :: Row -> [T.Text]
    rowToCells (Row syms) = [s | Symbol s <- map getSym (NE.toList syms)]
      where
        getSym (Symbol t) = Symbol t

    safeHead :: [a] -> Maybe a
    safeHead (x : _) = Just x
    safeHead _ = Nothing

boxTable :: [[T.Text]] -> T.Text
boxTable rows
  | null rows = ""
  | otherwise =
      let widths = columnWidths rows
          topBorder = "┌" <> hline "┬" "┐" widths
          midBorder = "├" <> hline "┼" "┤" widths
          botBorder = "└" <> hline "┴" "┘" widths
          rendered = map (renderRow widths) rows
       in T.unlines $ [topBorder] <> intersperseWith midBorder rendered <> [botBorder]

renderRow :: [Int] -> [T.Text] -> T.Text
renderRow widths cells =
  "│ " <> T.intercalate " │ " (zipWith pad widths cells) <> " │"

pad :: Int -> T.Text -> T.Text
pad n t = t <> T.replicate (n - T.length t) " "

columnWidths :: [[T.Text]] -> [Int]
columnWidths rs =
  [maximum (map T.length col) | col <- transpose rs]

hline :: T.Text -> T.Text -> [Int] -> T.Text
hline mid end widths =
  T.intercalate mid (map (\w -> T.replicate (w + 2) "─") widths) <> end

intersperseWith :: T.Text -> [T.Text] -> [T.Text]
intersperseWith _ [] = []
intersperseWith _ [x] = [x]
intersperseWith sep (x : xs) = x : sep : intersperseWith sep xs
