{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Definitions for the QTheseus source language AST.
module QTheseus.Syntax where

import Data.String (IsString)
import Data.Text (Text)

type Prog = [Def]

-- | Top-level language definitions
data Def
  = -- | Algebraic datatype declaration.
    DataTyp TName [(CName, Typ)]
  | -- | A named isomorphism.
    Iso FName ITyp [Clause]
  | -- | Evaluation of a isomorphism from left-to-right given a value.
    Eval FName Val
  deriving (Show, Eq)

-- | The types of the language.
data Typ
  = Zero
  | One
  | Times Typ Typ
  | Plus Typ Typ
  | -- | Polymorphic type variable
    TypVar VName
  | -- | User defined type
    TypDef TName
  deriving (Show, Eq, Ord)

-- | The values of the language.
data PVal
  = Unit
  | Prod Val Val
  | SumL Val
  | SumR Val
  | -- | Pattern starting with constructor.
    Constr CName PVal
  | -- | Variable pattern.
    Var VName
  | -- | Function application.
    App FName PVal
  deriving (Show, Eq)

-- | Give each constructor of `PVal` an (almost unique) number,
-- only letting `Var` and `App` have the same.
pvalToNum :: PVal -> Int
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
comparePVal :: PVal -> PVal -> Ordering
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

-- | Type isomorphism.
data ITyp = ITyp Typ Typ
  deriving (Show, Eq)

-- | A clause in a function is an isomorphism between two values.
data Clause = Clause PVal PVal
  deriving (Show, Eq)

-- | Function (isomorphism) name.
newtype FName = FName Text
  deriving (Show, Eq, Ord, IsString)

-- | Variable names.
newtype VName = VName Text
  deriving (Show, Eq, Ord, IsString)

-- | Data type name, or type variable.
newtype TName = TName Text
  deriving (Show, Eq, Ord, IsString)

-- | Constructor name.
newtype CName = CName Text
  deriving (Show, Eq, Ord, IsString)
