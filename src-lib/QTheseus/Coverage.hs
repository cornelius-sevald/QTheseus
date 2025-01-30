{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}

{- Coverage checking of patterns.
 - For an isomorphism, the collection of clauses on the
 - left- and right-hand side must be a complete non-overlapping
 - covering of the input- and output types respectively.
 - -}
module QTheseus.Coverage (checkCoverage, reportError, reportErrors) where

import Data.List ((\\))
import qualified Data.List as L
import Debug.Trace (traceShow)
import Prettyprinter
import QTheseus.Core
import QTheseus.Pretty ()
import QTheseus.Syntax
import Util

debug :: Bool
debug = False

-- | Trace a value with optional debug messaging,
-- depending on whether or not `debug` is on.
traceMsg :: (Show a) => Bool -> a -> b -> b
traceMsg on msg = if on then traceShow msg else id

type Constructors vn = [Constructor vn]

type TypEnv vn = [(vn, Constructors vn)]

type CurTyp vn = (Typ vn, TypEnv vn)

-- | Lookup a constructor by type- and constructor name.
lookupConstr :: (Eq vn) => vn -> vn -> TypEnv vn -> Either (CoverageError vn) (Typ vn)
lookupConstr c t env = do
  cs <- lookupTyp t env
  let cs' = (\(Constructor name typ) -> (name, typ)) <$> cs
  case lookup c cs' of
    Nothing -> Left $ UnboundConstr c t cs
    Just t' -> Right t'

-- | Lookup a type by name.
lookupTyp :: (Eq vn) => vn -> TypEnv vn -> Either (CoverageError vn) (Constructors vn)
lookupTyp t env = case lookup t env of
  Nothing -> Left $ UnboundTyp t env
  Just cs -> Right cs

type Context = String

data CoverageError vn
  = -- | No matches on side of isomorphism.
    NoMatches (PVal vn)
  | -- | Multiple matches on side of isomorphism.
    MultipleMatches (PVal vn)
  | -- | Duplicates variable in pattern.
    DupsVariable vn
  | -- | Drops existing variable on side of clause.
    DropsVariable vn
  | UnboundTyp vn (TypEnv vn)
  | UnboundConstr vn vn (Constructors vn)
  deriving (Show, Eq)

-- instance (Pretty vn) => Pretty (CoverageError vn) where
--  pretty = viaShow

-- | Check the coverage of a program,
-- giving a list of coverage errors encountered.
checkCoverage :: UncheckedProg -> [(Context, CoverageError Name)]
checkCoverage prog = go prog []
  where
    -- Loop through the program and report any coverage errors.
    go :: [Def Name] -> TypEnv Name -> [(Context, CoverageError Name)]
    -- No definitions => no errors.
    go [] _ = []
    go (def : defs) env =
      case def of
        Eval _ _ -> go defs env
        -- When encountering a type declaration,
        -- we add the type to the environment.
        DataTyp name args -> go defs ((name, L.sort args) : env)
        -- For a type isomorphism, we process the left- and right-hand side separately.
        Iso name ityp clauses ->
          let lhs = checkSide LHS name clauses ityp env
              rhs = checkSide RHS name clauses ityp env
              vrs = concatMap (checkVars name) (zip [1 ..] clauses)
           in lhs ++ rhs ++ vrs ++ go defs env

-- | Check the coverage one side of an isomorphism.
checkSide ::
  Side ->
  Name ->
  [Clause Name] ->
  ITyp Name ->
  TypEnv Name ->
  [(Context, CoverageError Name)]
checkSide side name clauses ityp env =
  let typ = projITyp side ityp
      pats = projClause side <$> clauses
      context = show (pretty side <+> "of" <+> pretty name)
      coverage = exhaustive context pats (typ, env)
   in (context,) <$> coverage

-- | Check that all variables are used exactly once
-- on each side of a clause.
-- Also checks for duplicate variable names on the same side.
checkVars :: Name -> (Integer, Clause Name) -> [(Context, CoverageError Name)]
checkVars name (n, clause) =
  let lhsVars = vars $ projClause LHS clause
      rhsVars = vars $ projClause RHS clause
      lhsDups = traceMsg debug ("LHS vars: " ++ show lhsVars) $ findDupVars lhsVars
      rhsDups = traceMsg debug ("RHS vars: " ++ show rhsVars) $ findDupVars rhsVars
      lhsDrops = findDropVars lhsVars rhsVars
      rhsDrops = findDropVars rhsVars lhsVars
      ctx side = show (pretty side <+> "of clause" <+> pretty n <+> "of" <+> pretty name)
   in ((ctx LHS,) . DupsVariable <$> lhsDups)
        ++ ((ctx RHS,) . DupsVariable <$> rhsDups)
        ++ ((ctx LHS,) . DropsVariable <$> lhsDrops)
        ++ ((ctx RHS,) . DropsVariable <$> rhsDrops)
  where
    -- The set of duplicate variables in `xs`.
    findDupVars :: [Name] -> [Name]
    findDupVars xs = L.nub (xs \\ L.nub xs)
    -- The set of variables in `ys` not in `xs`,
    -- i.e. the set of variables dropped from `xs` w.r.t. `ys`.
    findDropVars :: [Name] -> [Name] -> [Name]
    findDropVars xs ys = L.nub ys \\ L.nub xs
    -- Extract all variables of a pattern.
    vars :: PVal Name -> [Name]
    vars Unit = []
    vars (Prod p1 p2) = vars p1 ++ vars p2
    vars (SumL pl) = vars pl
    vars (SumR pr) = vars pr
    vars (Constr _ ps) = vars ps
    vars (App _ ps) = vars ps
    vars (Var v) = [v]

-- | Check if a list of patterns are exhaustive for a type.
exhaustive ::
  Context ->
  [PVal Name] ->
  CurTyp Name ->
  [CoverageError Name]
exhaustive context ps ct =
  let ps' = L.sortBy comparePVal ps
      res = covers [wildcard] ps' ct
      msg = "! Checking" <+> pretty context <+> ":" <+> pretty ps'
   in traceMsg debug msg res

-- | Check if two lists of patterns exactly cover each other
covers ::
  [PVal Name] ->
  [PVal Name] ->
  CurTyp Name ->
  [CoverageError Name]
covers [] [] _ = []
covers [] ps _ = MultipleMatches <$> ps
covers ps [] _ = NoMatches <$> ps
covers (a : as) (b : bs) ct = either pure trace_next (reconcile a b ct)
  where
    next ([_], [_]) = trace_covers as bs ct
    next (a1 : as1, b1 : bs1) = trace_covers (a1 : as1 ++ as) (b1 : bs1 ++ bs) ct
    next (_, _)
      | comparePVal a b == LT = NoMatches a : covers as (b : bs) ct
      | otherwise = MultipleMatches b : covers (a : as) bs ct
    -- For optionally debug
    trace_next vs =
      let msg = "! next" <+> pretty vs
       in traceMsg debug msg (next vs)
    trace_covers as' bs' ct' =
      let msg = "! cover" <+> parens (pretty as') <+> parens (pretty bs')
       in traceMsg debug msg (covers as' bs' ct')

-- | Check if two patterns are the same
-- if they will satisfy exactly the same values, then they are returned.
-- if one is more general than the other, then it is split up into
--   more specific subpatterns
-- if they can't be reconciled then no patterns are returned
reconcile ::
  PVal Name ->
  PVal Name ->
  CurTyp Name ->
  Either (CoverageError Name) ([PVal Name], [PVal Name])
reconcile x y _
  | matchesAny x && matchesAny y = pure ([x], [y])
reconcile x p typ
  | matchesAny x = sequenceTup2 (expand p typ, pure [p])
reconcile p y typ
  | matchesAny y = sequenceTup2 (pure [p], expand p typ)
reconcile Unit Unit (One, _) = pure ([Unit], [Unit])
reconcile (Prod p1 p2) (Prod p1' p2') (Times t1 t2, env) = do
  (r1, r1') <- reconcile p1 p1' (t1, env)
  (r2, r2') <- reconcile p2 p2' (t2, env)
  let x1 = Prod <$> r1 <*> r2
  let x2 = Prod <$> r1' <*> r2'
  pure (x1, x2)
reconcile (SumL p1) (SumL p2) (Plus t _, env) = do
  (r1, r2) <- reconcile p1 p2 (t, env)
  let msg = "! reconcile" <+> pretty (p1, p2) <+> "=" <+> pretty (r1, r2)
  let x1 = SumL <$> r1
  let x2 = SumL <$> r2
  pure $ traceMsg debug msg (x1, x2)
reconcile (SumR p1) (SumR p2) (Plus _ t, env) = do
  (r1, r2) <- reconcile p1 p2 (t, env)
  let x1 = SumR <$> r1
  let x2 = SumR <$> r2
  pure (x1, x2)
reconcile (Constr c1 p1) (Constr c2 p2) (TypDef t, env)
  | c1 == c2 = do
      let c = c1 -- `c1` and `c2` are equal, so we arbitrarily use `c1`.
      t' <- lookupConstr c t env
      (r1, r2) <- reconcile p1 p2 (t', env)
      let x1 = Constr c1 <$> r1
      let x2 = Constr c2 <$> r2
      pure (x1, x2)
reconcile _ _ _ = pure ([], [])

-- | Expand out patterns of the type till the granularity of the
-- specified pattern is achieved.
--
-- NOTE: not sure exactly how this interacts with `TypVar`,
-- as Theseus doesn't distinguish between
-- type variables and user-defined types.
expand ::
  PVal Name ->
  CurTyp Name ->
  Either (CoverageError Name) [PVal Name]
expand Unit (One, _) = pure [Unit]
expand x (One, _)
  | matchesAny x = pure [Unit]
expand x (_, _)
  | matchesAny x = pure [x]
expand (Prod p1 p2) (Times t1 t2, env) = do
  vs1 <- expand p1 (t1, env)
  vs2 <- expand p2 (t2, env)
  pure $ Prod <$> vs1 <*> vs2
expand (SumL p) (Plus t _, env) = do
  vs <- expand p (t, env)
  pure $ fmap SumL vs ++ [SumR wildcard]
expand (SumR p) (Plus _ t, env) = do
  vs <- expand p (t, env)
  pure $ [SumL wildcard] ++ fmap SumR vs
expand (Constr c p) (TypDef t, env) = do
  constrs <- lookupTyp t env
  fmap concat . sequenceA $ do
    Constructor c' t' <- constrs
    if c == c'
      then pure $ fmap (Constr c') <$> expand p (t', env)
      else
        if t' == One
          then [pure [Constr c' Unit]]
          else [pure [Constr c' wildcard]]
expand _ _ = pure []

-- | Wildcard pattern matching any
wildcard :: PVal Name
wildcard = Var "_"

-- | Check if a pattern matches any value.
matchesAny :: PVal vn -> Bool
matchesAny (Var _) = True
matchesAny (App _ _) = True
matchesAny _ = False

reportError :: (Pretty vn) => Context -> CoverageError vn -> Doc ann
reportError context err =
  case err of
    (MultipleMatches p) ->
      "Error:"
        <+> pretty context
        <> ":"
        <+> "Multiple patterns match values of the form"
        <+> quote p
        <> "."
    (NoMatches p) ->
      "Error:"
        <+> pretty context
        <> ":"
        <+> "No patterns match values of the form"
        <+> quote p
        <> "."
    (DupsVariable v) ->
      "Error:"
        <+> pretty context
        <> ":"
        <+> "Duplicates variable"
        <+> quote v
        <> "."
    (DropsVariable v) ->
      "Error:"
        <+> pretty context
        <> ":"
        <+> "Drops variable"
        <+> quote v
        <> "."
    (UnboundTyp t env) ->
      "Error:"
        <+> pretty context
        <> ":"
        <+> "Could not find type"
        <+> quote t
        <+> "in list of bound types:"
        <+> pretty (fst <$> env)
    (UnboundConstr t c constrs) ->
      do
        "Error:"
        <+> pretty context
        <> ":"
        <+> "Could not find constructor "
        <+> quote c
        <+> "of type"
        <+> quote t
        <+> "in list of constructors:"
        <+> pretty ((\(Constructor name _) -> name) <$> constrs)
  where
    quote x = surround (pretty x) "'" "'"

reportErrors :: (Pretty vn) => [(Context, CoverageError vn)] -> Doc ann
reportErrors = vcat . map (uncurry reportError)
