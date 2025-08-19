{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Gigalog.Evaluator (Row (..), evaluate, EvaluationEnv (..)) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Parallel.Strategies (using, rdeepseq, parListChunk)
import Data.Foldable (toList, Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Gigalog.Data.Common (PredName, Symbol, VarName (VarName), Tables, Row (Row))
import Gigalog.Syntax.AST (Atom (Atom), Fact (Fact), Program (Program, rules, facts), Rule (Rule), Term (..))
import Data.Traversable (for)
import Data.Function (fix)

-- we define a database as a mapping from a predicate name to a list of rows
-- which are associated with that predicate

data EvaluationEnv = EvaluationEnv
  { full :: Tables,
    delta :: Tables,
    idbPredicates :: Set.Set PredName,
    edbPredicates :: Set.Set PredName
  }
  deriving (Eq, Ord, Show)

----------------------------------------------
-- Evaluation
---------------------------------------------

initializeDB :: Program -> Except T.Text EvaluationEnv
initializeDB program@(Program { facts, rules }) =
  let -- all of the predicate names for facts
      factPredicates = Set.fromList [p | Fact p _ <- facts]

      -- a set of body predicate names
      bodies = Set.fromList [p | Rule _ body <- rules, Atom p _ <- toList body]

      -- the IDB is essentially all rules that appear in some rule head
      idb = Set.fromList [p | Rule (Atom p _) _ <- rules]

      -- the EDB is essentially all predicates with a corresponding
      -- fact that don't appear in the rule head
      e = (bodies `Set.union` factPredicates) `Set.difference` idb

      -- obtain all the idb facts
      idbEntries = Map.fromSet (const Set.empty) idb

      -- obtain all of the EDB facts
      edbEntries =
        foldl'
          ( \m (Fact p syms) ->
              Map.insertWith Set.union p (Set.singleton (Row syms)) m
          )
          (Map.fromSet (const Set.empty) e)
          facts
   in do
    -- obtain the initial delta
    d <- initialDelta program e edbEntries

    -- create the evaluation environment
    pure $ EvaluationEnv
        { full = Map.unionWith Set.union idbEntries edbEntries,
          delta = Map.unionWith Set.union idbEntries d,
          idbPredicates = idb,
          edbPredicates = e
        }

-- \delta_{S}^{1} := P'(I)(S), for each IDB predicate S;
initialDelta :: Program -> Set PredName -> Tables -> Except T.Text Tables
initialDelta (Program _ rules) e table =
  foldM
    ( \m rule@(Rule (Atom p _) body) ->
        if all (`Set.member` e) [q | Atom q _ <- toList body]
          then do
            newRows <- evalRuleInitial table rule
            pure $ Map.insertWith Set.union p newRows m
          else pure m
    )
    Map.empty
    rules

evalRuleInitial :: Tables -> Rule -> Except T.Text (Set.Set Row)
evalRuleInitial table (Rule (Atom _ ruleTerms) bodyNE) = do
    let body = NonEmpty.toList bodyNE
        pairs = [ (a, Map.findWithDefault Set.empty p table) | a@(Atom p _) <- body ]
        newBindings = joinBody pairs
        in projectHead newBindings ruleTerms

projectHead :: Set Bindings -> NonEmpty Term -> Except T.Text (Set Row)
projectHead bindings headTerms = do
  rowsNE <- mapM (projectOne headTerms) (Set.toList bindings)
  pure $ Set.fromList (map Row rowsNE)
  where
    projectOne :: NonEmpty Term -> Bindings -> Except T.Text (NonEmpty Symbol)
    projectOne terms b = traverse (termToSym b) terms

    termToSym :: Bindings -> Term -> Except T.Text Symbol
    termToSym b = \case
      TSym s -> pure s
      TVar v@(VarName name) ->
        case Map.lookup v b of
          Just s -> pure s
          Nothing -> throwE $ "INTERPRETER BUG - variable " <> name <> " missing in binding"

type Bindings = Map.Map VarName Symbol

seminaiveJoinPlan :: Set.Set PredName
  -> Tables
  -> Tables
  -> Rule
  -> Except T.Text [[(Atom, Set.Set Row)]]
seminaiveJoinPlan idb fullTbl deltaTbl (Rule _ bodyNE) = do
  let atoms  = NonEmpty.toList bodyNE
      idbIdx = [ i | (i, Atom p _) <- zip [0..] atoms, p `Set.member` idb ]

      fetch :: Tables -> PredName -> Except T.Text (Set.Set Row)
      fetch tbl p =
        case Map.lookup p tbl of
          Just rows -> pure rows
          Nothing -> throwE "seminaiveJoinPlan: missing predicate in tables"

      planFor :: Int -> Except T.Text [(Atom, Set.Set Row)]
      planFor i =
        traverse
          (\(n, atom@(Atom p _)) -> do
              let tbl = if n == i then deltaTbl else fullTbl
              rows <- fetch tbl p
              pure (atom, rows)
          )
          (zip [0..] atoms)

  traverse planFor idbIdx

seminaiveStep :: Program -> EvaluationEnv -> Except T.Text EvaluationEnv
seminaiveStep (Program { rules }) (EvaluationEnv { full, delta, idbPredicates, edbPredicates }) = do
  -- remember that the full database is actually the one from the last iteration.
  let fullDB = Map.unionWith Set.union full delta

  -- we need to join the stuff for the rules
  bindingRulePairs <- for rules \rule@(Rule (Atom predName ruleTerms) _) -> do
    bindings <- seminaiveJoin idbPredicates full delta rule
    newRows <- projectHead bindings ruleTerms
    pure (predName, newRows)

  -- boom. Now we have the new rules from this step
  let newRules = Map.fromListWith Set.union bindingRulePairs
  let deltaS = diffTables newRules fullDB
  let full' = Map.unionWith Set.union fullDB deltaS

  pure $ EvaluationEnv
    { full = full'
    , delta = deltaS
    , idbPredicates = idbPredicates
    , edbPredicates = edbPredicates
    }
  where
  diffTables :: Tables -> Tables -> Tables
  diffTables = Map.differenceWith diff
    where
      diff s1 s2 =
        let d = Set.difference s1 s2
        in Just d

seminaiveJoin :: Set.Set PredName
  -> Tables
  -> Tables
  -> Rule
  -> Except T.Text (Set Bindings)
seminaiveJoin idb fullTbl deltaTbl rule = do
  plans <- seminaiveJoinPlan idb fullTbl deltaTbl rule
  let parts = map joinBody plans `using` parListChunk greenThreads rdeepseq
  pure (Set.unions parts)

joinBody :: [(Atom, Set Row)] -> Set Bindings
joinBody = foldl' step (Set.singleton Map.empty)
  where
    {-# INLINE step #-}
    step acc (atom, rows) =
      let bindings = Set.toList acc
          chunks = map (unifyAtom rows atom) bindings
                    `using` parListChunk greenThreads rdeepseq
      in 
      Set.unions chunks

{-# INLINE unifyAtom #-}
unifyAtom :: Set Row -> Atom -> Bindings -> Set Bindings
unifyAtom rows atom bindings =
    let rowList = Set.toList rows
    in Set.fromList $ mapMaybe (unifyEntry atom) rowList
  where
    unifyEntry :: Atom -> Row -> Maybe Bindings
    unifyEntry (Atom _ terms) (Row symbols) = unifySymbols (toList terms) (toList symbols)

    unifySymbols :: [Term] -> [Symbol] -> Maybe Bindings
    unifySymbols terms symbols
      | length terms == length symbols = foldM step bindings $ zip terms symbols
      | otherwise = Nothing
      where
        step = unify

    {-# INLINE unify #-}
    unify :: Bindings -> (Term, Symbol) -> Maybe Bindings
    unify b (t, s) = case t of
      TVar varName ->
        case Map.lookup varName b of
          Nothing -> pure $ Map.insert varName s b
          Just sym | sym == s -> pure b
          _ -> Nothing
      TSym sym | sym == s -> pure b
      _ -> Nothing

solve :: Program -> EvaluationEnv -> Except T.Text EvaluationEnv
solve program = 
  fix $ \recur env -> do
    env' <- seminaiveStep program env
    if fixpointReached env' then pure env' else recur env'
  where
  fixpointReached :: EvaluationEnv -> Bool
  fixpointReached (EvaluationEnv { delta }) = all Set.null $ Map.elems delta 

evaluate :: Program -> Except T.Text EvaluationEnv
evaluate program = do
  initialEnv <- initializeDB program
  solve program initialEnv

greenThreads :: Int
greenThreads = 512
