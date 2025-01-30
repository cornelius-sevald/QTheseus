{-# LANGUAGE OverloadedStrings #-}

-- | Type checking and type inference.
module QTheseus.TypeChecker (TypeError, checkDupDefs, makeNamesUnique) where

import Control.Monad.State (runState)
import Data.List (find, nub, (\\))
import Prettyprinter (pretty)
import QTheseus.Core
import QTheseus.FreshNames
import QTheseus.Pretty ()
import QTheseus.Syntax

type Prog = ProgBase VName

data TypeError vn
  = DupeDefinition vn
  | RecursiveTyp vn
  | RecursiveIso vn
  deriving (Show, Eq)

{- Unification of terms. Algorithm taken from
 - https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm
 -}

-- data Term vn
--   = TVar vn
--   | Term vn [Term vn]
--   deriving (Show, Eq)
--
-- -- | Invariant for substitutions:
-- -- no name on a lhs occurs in any term earlier in the list.
-- type Substitution vn = [(vn, Term vn)]
--
-- -- | Check if a variable occurs in a term.
-- occurs :: (Eq vn) => vn -> Term vn -> Bool
-- occurs x (TVar y) = x == y
-- occurs x (Term _ s) = any (occurs x) s
--
-- -- | Substitute term `s` for all occurrences of variable `x` in term `t`.
-- subst :: (Eq vn) => Term vn -> vn -> Term vn -> Term vn
-- subst s x t@(TVar y)
--   | x == y = s
--   | otherwise = t
-- subst s x (Term f u) = Term f $ map (subst s x) u
--
-- -- | Apply a substitution right to left.
-- apply :: (Eq vn) => Substitution vn -> Term vn -> Term vn
-- apply s t = foldr (\(x, u) -> subst u x) t s
--
-- -- | Unify one pair.
-- unify1 :: (Eq vn) => Term vn -> Term vn -> Either (UnificationError vn) (Substitution vn)
-- unify1 (TVar x) t@(TVar y)
--   | x == y = pure []
--   | otherwise = pure [(x, t)]
-- unify1 (Term f sc) (Term g tc)
--   | f == g && length sc == length tc = unify (zip sc tc)
--   | otherwise = Left $ SymbolMismatch f g
-- unify1 (TVar x) t@(Term _ _)
--   | occurs x t = Left $ OccursCheck x t
--   | otherwise = pure [(x, t)]
-- unify1 s@(Term _ _) (TVar y)
--   | occurs y s = Left $ OccursCheck y s
--   | otherwise = pure [(y, s)]
--
-- -- | Unify a list of pairs.
-- unify :: (Eq vn) => [(Term vn, Term vn)] -> Either (UnificationError vn) (Substitution vn)
-- unify [] = pure []
-- unify ((x, y) : t) = do
--   t2 <- unify t
--   t1 <- unify1 (apply t2 x) (apply t2 y)
--   pure $ t1 ++ t2

{- Duplicate definitions -}

checkDupDefs :: (Eq vn) => ProgBase vn -> Maybe (TypeError vn)
checkDupDefs prog =
  case dupNames of
    [] -> Nothing
    (name : _) -> Just $ DupeDefinition name
  where
    dupNames = dupTypNames ++ dupConstrNames ++ dupIsoNames
    dupTypNames = typNames \\ nub typNames
    dupConstrNames = constrNames \\ nub constrNames
    dupIsoNames = isoNames \\ nub isoNames
    typNames = getTypeNames prog
    constrNames = getConstructorNames prog
    isoNames = getIsoNames prog

{- Making names globally unique -}

makeNamesUnique :: VNameSource -> UncheckedProg -> (Prog, VNameSource)
makeNamesUnique src prog = flip runState src $ do
  typNames <- mapM newNameM $ getTypeNames prog
  constrNames <- mapM newNameM $ getConstructorNames prog
  isoNames <- mapM newNameM $ getIsoNames prog
  mapM (renameDef typNames constrNames isoNames) prog

renameDef :: (MonadFreshNames m) => [VName] -> [VName] -> [VName] -> Def Name -> m (Def VName)
renameDef typNames constrNames isoNames def = case def of
  DataTyp name constrs -> do
    let name' = rename name typNames
    constrs' <- mapM renameConstr constrs
    pure $ DataTyp name' constrs'
  Iso name ityp clauses -> do
    let name' = rename name isoNames
    ityp' <- renameITyp ityp
    clauses' <- mapM renameClause clauses
    pure $ Iso name' ityp' clauses'
  Eval name val -> do
    vars <- mapM newNameM $ getVars val
    let name' = rename name isoNames
    let val' = renameVal isoNames constrNames vars val
    pure $ Eval name' val'
  where
    renameConstr (Constructor name typ) = do
      -- Parsing should make sure there are no type variables,
      -- but we can't statically guarantee that.
      typVars <- mapM newNameM $ getTypVars typ
      let name' = rename name constrNames
      let typ' = renameTyp typNames typVars typ
      pure $ Constructor name' typ'
    renameITyp (ITyp tl tr) = do
      typVars <- mapM newNameM $ nub (getTypVars tl ++ getTypVars tr)
      let tl' = renameTyp typNames typVars tl
      let tr' = renameTyp typNames typVars tr
      pure $ ITyp tl' tr'
    renameClause (Clause vl vr) = do
      vars <- mapM newNameM $ nub (getVars vl ++ getVars vr)
      let vl' = renameVal isoNames constrNames vars vl
      let vr' = renameVal isoNames constrNames vars vr
      pure $ Clause vl' vr'

renameTyp :: [VName] -> [VName] -> Typ Name -> Typ VName
renameTyp typNames typVars typ =
  case typ of
    Zero -> Zero
    One -> One
    TypVar name -> TypVar $ rename name typVars
    TypDef name -> TypDef $ rename name typNames
    Times t1 t2 ->
      let t1' = renameTyp typNames typVars t1
          t2' = renameTyp typNames typVars t2
       in Times t1' t2'
    Plus t1 t2 ->
      let t1' = renameTyp typNames typVars t1
          t2' = renameTyp typNames typVars t2
       in Plus t1' t2'

renameVal :: [VName] -> [VName] -> [VName] -> PVal Name -> PVal VName
renameVal isoNames constrNames vars val =
  case val of
    Unit -> Unit
    Var name -> Var $ rename name vars
    Constr name v ->
      let name' = rename name constrNames
          v' = renameVal isoNames constrNames vars v
       in Constr name' v'
    App name v ->
      let name' = rename name isoNames
          v' = renameVal isoNames constrNames vars v
       in App name' v'
    SumL v -> SumL $ renameVal isoNames constrNames vars v
    SumR v -> SumR $ renameVal isoNames constrNames vars v
    Prod v1 v2 ->
      let v1' = renameVal isoNames constrNames vars v1
          v2' = renameVal isoNames constrNames vars v2
       in Prod v1' v2'

rename :: Name -> [VName] -> VName
rename name vnames =
  case find (\vname -> baseName vname == name) vnames of
    Just vname -> vname
    Nothing -> error $ show ("No name '" <> pretty name <> "' found in " <> pretty vnames)
