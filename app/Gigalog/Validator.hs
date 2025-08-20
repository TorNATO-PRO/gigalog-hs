{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gigalog.Validator (preprocess) where

import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.State (State, evalState, get, put, runState, StateT, execStateT)
import Data.Foldable (for_, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (for)
import Gigalog.Data.Common (PredName (PredName), VarName (VarName))
import Gigalog.Syntax.AST (Atom (Atom), Program (Program), Rule (Rule), Term (..), Fact (Fact))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.Trans.Class (MonadTrans(lift))

----------------------------------------------
-- Preprocessing
----------------------------------------------

preprocess :: Program -> Except T.Text Program
preprocess = renameVariables
  >=> allHeadVariablesFoundInBody
  >=> atomAritiesMatch

-- We need some mechanism to rename all variables. Here I use the state monad
-- to keep track of a counter and we append the integer to the end, to reduce conflicts.
-- We want our variables to be globally unique :)

data RenameEnv = RenameEnv {counter :: !Int, mapping :: Map.Map VarName VarName}

renameVariables :: Program -> Except T.Text Program
renameVariables (Program facts rules) = do
  let step r = do
        c <- get
        let (r', env') = runState (renameRule r) (RenameEnv c Map.empty)
        put (counter env')
        pure r'
  let newRules = evalState (traverse step (Set.toList rules)) 0
  pure $ Program facts $ Set.fromList newRules
  where
    renameRule :: Rule -> State RenameEnv Rule
    renameRule (Rule ruleHead body) = do
      head' <- renameAtom ruleHead
      body' <- traverse renameAtom body
      pure $ Rule head' body'

    renameAtom :: Atom -> State RenameEnv Atom
    renameAtom (Atom predName body) = Atom predName <$> for body renameTerm

    renameTerm :: Term -> State RenameEnv Term
    renameTerm = \case
      TVar var -> TVar <$> renameVariable var
      other -> pure other

    renameVariable :: VarName -> State RenameEnv VarName
    renameVariable variable = do
      renameEnv <- get
      let rewriteMap = mapping renameEnv
      let c = counter renameEnv
      case Map.lookup variable rewriteMap of
        Just newVariable -> pure newVariable
        Nothing -> do
          let newVariable = bump variable c
          put $ RenameEnv (c + 1) (Map.insert variable newVariable rewriteMap)
          pure newVariable

    bump :: VarName -> Int -> VarName
    bump (VarName name) n = VarName (name <> T.pack (show n))

-- we need to validate that all variables present in the head relation of a
-- rule are also present in the body of a rule.

allHeadVariablesFoundInBody :: Program -> Except T.Text Program
allHeadVariablesFoundInBody program@(Program _ rules) = do
  for_ rules validateRule
  pure program
  where
    validateRule :: Rule -> Except T.Text ()
    validateRule (Rule (Atom (PredName headName) terms) body) =
      let headVariables = Set.fromList $ varsIn terms
          bodyVariables = Set.fromList $ concatMap (\(Atom _ ts) -> varsIn ts) body
       in unless (headVariables `Set.isSubsetOf` bodyVariables) $
            throwE $
              "Unsafe rule: variables in head relation " <> headName <> " that do not occur in the body."

    varsIn :: (Foldable f) => f Term -> [VarName]
    varsIn = mapMaybe (\case TVar v -> Just v; _ -> Nothing) . toList

-- all of the atoms should be of matching arities. If they are not,
-- then we have a problem

type Arity = Int
type ArityTable = Map.Map PredName Arity

atomAritiesMatch :: Program -> Except T.Text Program
atomAritiesMatch program@(Program facts rules) = do
  let go = do
        mapM_ checkFact facts
        mapM_ checkRule rules
  _tbl <- execStateT go Map.empty
  pure program
  where
  checkFact :: Fact -> StateT ArityTable (Except T.Text) ()
  checkFact (Fact p syms) = record "fact" p (NonEmpty.length syms) 

  checkRule :: Rule -> StateT ArityTable (Except T.Text) ()
  checkRule (Rule headAtom body) = do
    let Atom ph hts = headAtom
    record "rule head" ph (NonEmpty.length hts)
    mapM_ (\(Atom pb bts) -> record "rule body" pb (NonEmpty.length bts)) body

  record :: T.Text -> PredName -> Arity -> StateT ArityTable (Except T.Text) ()
  record ctx p@(PredName predName) n = do
    tbl <- get
    case Map.lookup p tbl of
      Nothing -> put (Map.insert p n tbl)
      Just n'
        | n' == n  -> pure ()
        | otherwise ->
            lift $ throwE $
              T.concat [ "arity mismatch for predicate "
                      , predName
                      , ": expected ", T.pack . show $ n'
                      , ", saw ", T.pack .show $ n
                      , " (in ", ctx, ")"
                      ]
