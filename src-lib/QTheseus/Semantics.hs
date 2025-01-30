{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module QTheseus.Semantics where

import Control.Exception (assert)
import Control.Monad (foldM, mapAndUnzipM)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Function (on)
import Data.List (find, sortBy)
import qualified Data.Matrix as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Vector as V
import Debug.Trace (trace)
import Prettyprinter
import QTheseus.Core
import qualified QTheseus.Pi as Pi
import QTheseus.Pretty ()
import QTheseus.Syntax
import Util

{- Evaluation using matrices -}

debugging :: Bool
debugging = False

debug :: String -> a -> a
debug = if debugging then trace else (\_ x -> x)

newtype EvalError vn = ConversionError (ToPiError vn)
  deriving (Show, Eq)

-- TODO: Convert the values back from vectors to Pi-, and then QTheseus, values.
eval :: (Ord vn, Pretty vn) => ProgBase vn -> Either (EvalError vn) [V.Vector Z]
eval prog_ = do
  debug (show $ "! eval\tprog_ = " <> pretty prog_) $ pure ()
  debug (show $ "! eval\tprog  = " <> pretty prog) $ pure ()
  debug (show $ "! eval\tdtyps = " <> pretty dtyps) $ pure ()
  debug (show $ "! eval\tisos  = " <> pretty isos) $ pure ()
  concat <$> mapM go prog
  where
    prog = inlineIsos $ expandIsos prog_
    isos = getIsoDefs prog
    dtyps = getTypeDefs prog
    go (Eval name val) = mapLeft ConversionError $ do
      let (ityp@(ITyp tl _), clauses) = lookup' name isos
      debug "! ====" $ pure ()
      debug (show $ "! eval.go\tname = " <> pretty name) $ pure ()
      debug (show $ "! eval.go\tval  = " <> pretty val) $ pure ()
      debug (show $ "! eval.go\tityp = " <> pretty ityp) $ pure ()
      mat <- isoToMat dtyps ityp clauses
      vec <- valToVec dtyps tl val
      debug (show $ "! eval.go\tmat  =\n" <> show mat) $ pure ()
      debug (show $ "! eval.go\tvec  =\n" <> show vec) $ pure ()
      debug "! ====" $ pure ()
      pure [M.getCol 1 $ mat @ vec]
    go _ = pure []

-- | Type used as matrix elements.
-- Subject to change once the rotation is implemented.
type Z = Int

-- | Matrix multiplication.
(@) :: (Num a) => M.Matrix a -> M.Matrix a -> M.Matrix a
(@) = M.multStd

{- Conversion to/from the QTheseus and Pi language -}

data ToQTError
  = InvalidPiValue Pi.Typ Pi.Val
  deriving (Show, Eq)

data ToPiError vn
  = DataTypeNotFound vn
  | InvalidQTType vn
  | InvalidQTValue (Typ vn) (Val vn)
  deriving (Show, Eq)

-- | Convert a Pi type `t` to a QTheseus type `t'`,
-- and obtain a function which converts Pi values of type `t`
-- to QTheseus values of type `t'`.
toQT ::
  Pi.Typ ->
  Either ToQTError (Typ vn, Pi.Val -> Either ToQTError (Val vn))
toQT t =
  case t of
    Pi.Zero -> do
      let t' = Zero
          f v = Left $ InvalidPiValue t v
      pure (t', f)
    Pi.One -> do
      let t' = One
          f Pi.Unit = pure Unit
          f v = Left $ InvalidPiValue t v
      pure (t', f)
    Pi.Times t1 t2 -> do
      (t1', f1) <- toQT t1
      (t2', f2) <- toQT t2
      let t' = Times t1' t2'
          f (Pi.Prod v1 v2) = Prod <$> f1 v1 <*> f2 v2
          f v = Left $ InvalidPiValue t v
      pure (t', f)
    Pi.Plus t1 t2 -> do
      (t1', f1) <- toQT t1
      (t2', f2) <- toQT t2
      let t' = Plus t1' t2'
          f (Pi.SumL v1) = SumL <$> f1 v1
          f (Pi.SumR v2) = SumR <$> f2 v2
          f v = Left $ InvalidPiValue t v
      pure (t', f)

-- | Convert a QTheseus type `t` to a Pi type `t'`,
-- and obtain a function which converts QTheseus values of type `t`
-- to Pi values of type `t'`.
--
-- TODO: This should also return a `Pi.Val -> Val` function,
-- which would recover the constructor names.
toPi ::
  (Eq vn) =>
  [(vn, [Constructor vn])] ->
  Typ vn ->
  Either (ToPiError vn) (Pi.Typ, Val vn -> Either (ToPiError vn) Pi.Val)
toPi dataTyps t =
  case t of
    Zero -> do
      let t' = Pi.Zero
          f v = Left $ InvalidQTValue t v
      pure (t', f)
    One -> do
      let t' = Pi.One
          f Unit = pure Pi.Unit
          f v = Left $ InvalidQTValue t v
      pure (t', f)
    Times t1 t2 -> do
      (t1', f1) <- toPi dataTyps t1
      (t2', f2) <- toPi dataTyps t2
      let t' = Pi.Times t1' t2'
          f (Prod v1 v2) = Pi.Prod <$> f1 v1 <*> f2 v2
          f v = Left $ InvalidQTValue t v
      pure (t', f)
    Plus t1 t2 -> do
      (t1', f1) <- toPi dataTyps t1
      (t2', f2) <- toPi dataTyps t2
      let t' = Pi.Plus t1' t2'
          f (SumL v1) = Pi.SumL <$> f1 v1
          f (SumR v2) = Pi.SumR <$> f2 v2
          f v = Left $ InvalidQTValue t v
      pure (t', f)
    TypDef name ->
      case lookup name dataTyps of
        Nothing -> Left $ DataTypeNotFound name
        -- When converting a custum datatype,
        -- we turn it into a sum type.
        Just constrs -> do
          let ts = map (\(Constructor _ typ) -> typ) constrs
          (ts', fs) <- mapAndUnzipM (toPi dataTyps) ts
          -- We let the sum be right associative.
          let t' = foldr Pi.Plus Pi.Zero ts'
          -- We loop over the constructors until we find a matching one.
          -- For each unmatched constructor, we nest a `SumR`, until the
          -- matching constructor is found, which is then nested in a `SumL`,
          -- corresponding to the right-associativity of `t'`.
          let f ((Constructor cname' _, g) : xs) v@(Constr cname val)
                | cname == cname' = Pi.SumL <$> g val
                | otherwise = Pi.SumR <$> f xs v
              f ((Constructor _ typ', _) : _) v = Left $ InvalidQTValue typ' v
              f [] v = Left $ InvalidQTValue Zero v
          pure (t', f $ zip constrs fs)
    TypVar name -> Left $ InvalidQTType name

{- Converting to/from QTheseus values and isomorphisms to vectors and matrices -}

valToVec ::
  (Eq vn) =>
  [(vn, [Constructor vn])] ->
  Typ vn ->
  Val vn ->
  Either (ToPiError vn) (M.Matrix Z)
valToVec dataTyps typ val = do
  (t', f) <- toPi dataTyps typ
  v' <- f val
  pure $ M.colVector $ piToVec t' v'

-- | Convert an isomorphisms (which has been inlined and expanded) into a matrix.
isoToMat ::
  (Eq vn) =>
  [(vn, [Constructor vn])] ->
  ITyp vn ->
  [Clause vn] ->
  Either (ToPiError vn) (M.Matrix Z)
isoToMat dataTyps ityp@(ITyp tL tR) clauses = do
  (tL', _) <- toPi dataTyps tL
  (tR', _) <- toPi dataTyps tR
  -- The size of the matrix is the size of the type on either
  -- side of the type isomorphism (which should both be the same).
  let n = assert (Pi.size tL' == (Pi.size tR' :: Int)) Pi.size tL'
  matL <- toMat n LHS
  matR <- toMat n RHS
  -- We should have that `matL` (and `matR` for that matter) are orthogonal
  let check = assert (matL @ M.transpose matL == M.identity n)
  pure $ check $ matR @ M.transpose matL
  where
    toMat n side = buildMat n <$> toVecs side
    buildMat n = foldl (M.<|>) (M.zero n 0)
    toVecs side =
      let t = projITyp side ityp
          vs = projClause side <$> clauses
       in mapM (valToVec dataTyps t) vs

piToVec :: Pi.Typ -> Pi.Val -> V.Vector Z
piToVec typ val =
  let size = Pi.size typ
      iso =
        debug ("! piToVec\ttyp = " <> show typ) $
          debug ("! piToVec\tval = " <> show val) $
            fromJust $
              Pi.simplify typ
      simpleVal = debug ("! piToVec\tiso = " <> show iso) fromJust $ (Pi.@@) iso val
      i = debug ("! piToVec\tsimpleVal = " <> show simpleVal) idx simpleVal
   in debug ("! piToVec\ti = " <> show i <> "\n") V.generate size (\j -> boolToInt (i == j))
  where
    idx Pi.Unit = 0
    idx (Pi.SumL Pi.Unit) = 0
    idx (Pi.SumR t) = succ $ idx t
    idx t = error $ "Can't generate index for type " <> show t

{- Expansion and inlining of isomorphisms -}

-- | Check if a value matches a pattern.
-- As the second argument is a `Val`,
-- it technically shouldn't be able to be a variable or function application.
-- For variables this should be fine, but there might
-- be a situation in which the function application doesn't work,
-- but for what we are using this for it should be fine.
matches :: (Eq vn) => Val vn -> PVal vn -> Maybe [(vn, Val vn)]
matches Unit Unit = Just []
matches (Prod v1 v2) (Prod p1 p2) = do
  m1 <- matches v1 p1
  m2 <- matches v2 p2
  pure $ m1 ++ m2
matches (SumL v1) (SumL p1) = matches v1 p1
matches (SumR v2) (SumR p2) = matches v2 p2
matches (Constr vn v) (Constr pn p) | pn == vn = matches v p
matches v (Var x) = pure [(x, v)]
matches (Var x) p = pure [(x, p)]
matches (App _ _) _ = error "Not implemented"
matches _ (App _ _) = error "Not impemented"
matches _ _ = Nothing

substituteVarM :: (Eq vn, Monad m) => PVal vn -> (vn, m (PVal vn)) -> m (PVal vn)
substituteVarM pat subst@(var, y) =
  case pat of
    Unit -> pure Unit
    Prod p1 p2 -> Prod <$> substituteVarM p1 subst <*> substituteVarM p2 subst
    SumL p1 -> SumL <$> substituteVarM p1 subst
    SumR p2 -> SumR <$> substituteVarM p2 subst
    Constr name p -> Constr name <$> substituteVarM p subst
    App name p -> App name <$> substituteVarM p subst
    Var x -> if x == var then y else pure pat

substituteVarsM :: (Eq vn, Monad m) => PVal vn -> [(vn, m (PVal vn))] -> m (PVal vn)
substituteVarsM = foldM substituteVarM

substituteVar :: (Eq vn) => PVal vn -> (vn, PVal vn) -> PVal vn
substituteVar pat (var, y) = runIdentity $ substituteVarM pat (var, pure y)

substituteVars :: (Eq vn) => PVal vn -> [(vn, PVal vn)] -> PVal vn
substituteVars = foldl substituteVar

-- | Get a list of all possible values that can inhabit this type.
-- TODO: add proper error handling for non-enumerable types.
enumerate :: (Eq vn, Pretty vn) => [(vn, [Constructor vn])] -> Typ vn -> [Val vn]
enumerate dataTyps typ =
  case typ of
    Zero -> []
    One -> pure Unit
    Times t1 t2 -> do
      v1 <- enumerate dataTyps t1
      v2 <- enumerate dataTyps t2
      pure $ Prod v1 v2
    Plus t1 t2 -> map SumL (enumerate dataTyps t1) ++ map SumR (enumerate dataTyps t2)
    TypDef name -> do
      Constructor cname t <- lookup' name dataTyps
      v <- enumerate dataTyps t
      pure $ Constr cname v
    t@(TypVar _) -> error $ show $ "Can't enumerate type variable " <> pretty t

-- | Expland the variables of each clause of an isomorphisms,
-- enumerating each possible value they could take, so that there are no more isomorphisms.
--
-- NOTE: Can't handle isomorphisms involving type variables (as those are non-enumerable),
-- and so the isomorphisms will have to be monomorphized.
expandIsos :: (Ord vn, Pretty vn) => ProgBase vn -> ProgBase vn
expandIsos prog = map expandIso prog
  where
    dataTyps = getTypeDefs prog
    isos = getIsoDefs prog
    expandIso def =
      case def of
        (Iso name ityp clauses) -> Iso name ityp $ concatMap (expandClause ityp) clauses
        _ -> def
    -- As any variable will (or at least should) have the same type on either side of a clause,
    -- and it must show up exactly once on either side, it must expand to the same values.
    -- However, if we naïvely expand the patterns of either side, different variables
    -- might be expanded in different orders.
    -- To solve this, we associate each variable with a list of patterns it should
    -- expand to, and then apply this expansion on either side for each variable
    -- in the same order with a fold over all variables.
    expandClause (ITyp tl tr) (Clause pl pr) =
      let expansionsL = varExpansions tl pl
          expansionsR = varExpansions tr pr
          sortE = sortBy (compare `on` fst)
          expansions = assert (sortE expansionsL == sortE expansionsR) $ sortE expansionsL
          expandSide = flip substituteVarsM expansions
       in zipWith Clause (expandSide pl) (expandSide pr)
    -- For each variable, get a list of patterns that it should expand to.
    varExpansions typ pat =
      case (typ, pat) of
        (_, Unit) -> []
        (Times t1 t2, Prod p1 p2) -> varExpansions t1 p1 <> varExpansions t2 p2
        (Plus t1 _, SumL p1) -> varExpansions t1 p1
        (Plus _ t2, SumR p2) -> varExpansions t2 p2
        -- For user types, we need to find the matching constructor
        -- and use the corresponding type.
        (TypDef name, Constr cname p) ->
          let constrs = lookup' name dataTyps
              Constructor _ t = fromJust $ find (\(Constructor cname' _) -> cname == cname') constrs
           in varExpansions t p
        -- For function applications we don't care about the output type,
        -- but only the input type of the application.
        (_, App name p) ->
          let (ITyp tl _, _) = lookup' name isos
           in varExpansions tl p
        (_, Var var) -> [(var, enumerate dataTyps typ)]
        (_, _) -> error $ show $ "Can't expand pattern '" <> pretty pat <> "' of type '" <> pretty typ <> "'"

-- | Inline the isomorphisms of a program.
--
-- Note that if any isomorphisms are (co-)recursive, this function will loop forever,
-- but the type-checker should catch that (in the future).
inlineIsos :: (Ord vn, Pretty vn) => ProgBase vn -> ProgBase vn
inlineIsos prog = inlinedProg
  where
    inlinedProg = map inlineIso prog
    -- Assuming there are no (co-)recursive isomorphisms,
    -- we should be able to use this when building the inlined program.
    inlinedIsos = getIsoDefs inlinedProg
    inlineIso def =
      case def of
        (Iso name ityp clauses) -> Iso name ityp $ map inlineClause clauses
        _ -> def
    inlineClause (Clause pl pr) = Clause (inlinePat pl) (inlinePat pr)
    inlinePat pat =
      case pat of
        Unit -> Unit
        Prod p1 p2 -> Prod (inlinePat p1) (inlinePat p2)
        SumL p1 -> SumL (inlinePat p1)
        SumR p2 -> SumR (inlinePat p2)
        Constr name p -> Constr name (inlinePat p)
        Var var -> Var var
        App name v ->
          let (_, clauses) = lookup' name inlinedIsos
              matchLHS clause = (clause,) <$> matches v (projClause LHS clause)
           in case mapMaybe matchLHS clauses of
                [(clause, env)] -> substituteVars (projClause RHS clause) env
                _ -> error $ show $ "No pattern matching '" <> pretty v <> "' in LHS of '" <> pretty name <> "'"

{- Error reporting -}

reportEvalError :: (Pretty vn) => EvalError vn -> Doc ann
reportEvalError (ConversionError err) =
  case err of
    (DataTypeNotFound name) ->
      "Error:"
        <+> "Data type not found"
        <+> quote name
        <> "."
    (InvalidQTType name) ->
      "Error:"
        <+> "Invalid type"
        <+> quote name
        <> "."
    (InvalidQTValue typ val) ->
      "Error:"
        <+> "Invalid value"
        <+> quote val
        <+> ", expected value of type"
        <+> quote typ
        <> "."
  where
    quote x = surround (pretty x) "'" "'"
