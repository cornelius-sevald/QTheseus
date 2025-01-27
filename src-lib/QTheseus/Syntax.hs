{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Definitions for the QTheseus source language AST.
module QTheseus.Syntax where

import QTheseus.Core

-- | A program with no type- or coverage checking.
type UncheckedProg = [Def Name]

-- | Base program type, polymorphic over names.
type ProgBase vn = [Def vn]

-- | Top-level language definitions
data Def vn
  = -- | Algebraic datatype declaration.
    DataTyp vn [Constructor vn]
  | -- | A named isomorphism.
    Iso vn (ITyp vn) [Clause vn]
  | -- | Evaluation of a isomorphism from left-to-right given a value.
    Eval vn (Val vn)
  deriving (Show, Eq)

-- | The types of the language.
data Typ vn
  = Zero
  | One
  | Times (Typ vn) (Typ vn)
  | Plus (Typ vn) (Typ vn)
  | -- | Polymorphic type variable
    TypVar vn
  | -- | User defined type
    TypDef vn
  deriving (Show, Eq, Ord)

-- | The values of the language.
data PVal vn
  = Unit
  | Prod (PVal vn) (PVal vn)
  | SumL (PVal vn)
  | SumR (PVal vn)
  | -- | Pattern starting with constructor.
    Constr vn (PVal vn)
  | -- | Variable pattern.
    Var vn
  | -- | Function application.
    App vn (PVal vn)
  deriving (Show, Eq)

-- | Give each constructor of `PVal` an (almost unique) number,
-- only letting `Var` and `App` have the same.
pvalToNum :: PVal vn -> Int
pvalToNum Unit = 0
pvalToNum (Prod _ _) = 1
pvalToNum (SumL _) = 2
pvalToNum (SumR _) = 3
pvalToNum (Constr _ _) = 4
pvalToNum (Var _) = -1
pvalToNum (App _ _) = -1

-- | This function is used to compare patterns
-- when doing coverage checking.
-- The reason this is not an `Ord` instance is because
-- we would not have `p1 == p2` iff. `compare p1 p2 == EQ`,
-- as this function considers *any* combination of
-- variables and function applications equal.
comparePVal :: (Ord vn) => PVal vn -> PVal vn -> Ordering
comparePVal p1 p2 =
  compare (pvalToNum p1) (pvalToNum p2) <> comp p1 p2
  where
    comp Unit Unit = EQ
    comp (Prod x1 x2) (Prod x1' x2') =
      comparePVal x1 x1' <> comparePVal x2 x2'
    comp (SumL x1) (SumL x2) = comparePVal x1 x2
    comp (SumR x1) (SumR x2) = comparePVal x1 x2
    comp (Constr c1 x1) (Constr c2 x2) =
      compare c1 c2 <> comparePVal x1 x2
    -- This case only happens if `p1` or `p2` are either `Var` or `App`,
    -- which we say are equal.
    comp _ _ = EQ

-- | This is intented to be just the value part of `PVal`,
-- but is not enforced.
type Val = PVal

-- | A data type constructor
data Constructor vn = Constructor vn (Typ vn)
  deriving (Show, Eq, Ord)

-- | Type isomorphism.
data ITyp vn = ITyp (Typ vn) (Typ vn)
  deriving (Show, Eq)

-- | A clause in a function is an isomorphism between two values.
data Clause vn = Clause (PVal vn) (PVal vn)
  deriving (Show, Eq)
