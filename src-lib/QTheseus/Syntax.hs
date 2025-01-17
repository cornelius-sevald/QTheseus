-- | Definitions for the QTheseus source language AST.
module QTheseus.Syntax where

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
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

-- | This is intented to be just the value part of `PVal`,
-- but is not enforced.
type Val = PVal

-- | Type isomorphism.
data ITyp = ITyp Typ Typ
  deriving (Show, Eq, Ord)

-- | A clause in a function is an isomorphism between two values.
data Clause = Clause PVal PVal
  deriving (Show, Eq, Ord)

-- | Function (isomorphism) name.
newtype FName = FName Text
  deriving (Show, Eq, Ord)

-- | Variable names.
newtype VName = VName Text
  deriving (Show, Eq, Ord)

-- | Data type name, or type variable.
newtype TName = TName Text
  deriving (Show, Eq, Ord)

-- | Constructor name.
newtype CName = CName Text
  deriving (Show, Eq, Ord)
