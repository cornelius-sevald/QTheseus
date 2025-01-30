-- | The types, values and combinators of the ∏ language.
module QTheseus.Pi where

import Control.Applicative (Alternative (empty))
import Control.Exception (assert)

data Typ
  = Zero
  | One
  | Times Typ Typ
  | Plus Typ Typ
  deriving (Show, Eq, Ord)

data Val
  = Unit
  | Prod Val Val
  | SumL Val
  | SumR Val
  deriving (Show, Eq, Ord)

-- | Pi combinators.
data Iso
  = -- Congruence
    Id
  | Sym Iso
  | (:.:) Iso Iso
  | (:*:) Iso Iso
  | (:+:) Iso Iso
  | -- (+) is associative, commutative, and has a unit
    PlusZeroL
  | PlusZeroR
  | CommutePlus
  | AssocPlusL
  | AssocPlusR
  | -- (*) is associative, commutative, and has a unit
    TimesOneL
  | TimesOneR
  | CommuteTimes
  | AssocTimesL
  | AssocTimesR
  | -- (*) distributes over (+)
    TimesZeroL
  | TimesZeroR
  | Distribute
  | Factor
  deriving (Show, Eq, Ord)

-- | Each isomorphisms `f : a <-> b` has an adjoint `g : b <-> a`
-- such that `f v = v'` implies `g v' = v`.
adjoint :: Iso -> Iso
adjoint Id = Id
adjoint (Sym f) = f
adjoint (f :.: g) = adjoint g :.: adjoint f
adjoint (f :*: g) = adjoint f :*: adjoint g
adjoint (f :+: g) = adjoint f :+: adjoint g
adjoint PlusZeroL = PlusZeroR
adjoint PlusZeroR = PlusZeroL
adjoint CommutePlus = CommutePlus
adjoint AssocPlusL = AssocPlusR
adjoint AssocPlusR = AssocPlusL
adjoint TimesOneL = TimesOneR
adjoint TimesOneR = TimesOneL
adjoint CommuteTimes = CommuteTimes
adjoint AssocTimesL = AssocTimesR
adjoint AssocTimesR = AssocTimesL
adjoint TimesZeroL = TimesZeroR
adjoint TimesZeroR = TimesZeroL
adjoint Distribute = Factor
adjoint Factor = Distribute

-- | Type isomorphisms of combinators.
(§§) :: Iso -> Typ -> Maybe Typ
Id §§ t = pure t
(Sym f) §§ t = adjoint f §§ t
(f :.: g) §§ t = (f §§ t) >>= (§§) g
(f :*: g) §§ (Times t1 t2) = Times <$> (f §§ t1) <*> (g §§ t2)
(f :+: g) §§ (Plus t1 t2) = Plus <$> (f §§ t1) <*> (g §§ t2)
PlusZeroL §§ (Plus Zero t2) = pure t2
PlusZeroR §§ t2 = pure (Plus Zero t2)
CommutePlus §§ (Plus t1 t2) = pure (Plus t2 t1)
AssocPlusL §§ (Plus t1 (Plus t2 t3)) = pure (Plus (Plus t1 t2) t3)
AssocPlusR §§ (Plus (Plus t1 t2) t3) = pure (Plus t1 (Plus t2 t3))
TimesOneL §§ (Times One t2) = pure t2
TimesOneR §§ t2 = pure (Times One t2)
CommuteTimes §§ (Times t1 t2) = pure (Times t2 t1)
AssocTimesL §§ (Times t1 (Times t2 t3)) = pure (Times (Times t1 t2) t3)
AssocTimesR §§ (Times (Times t1 t2) t3) = pure (Times t1 (Times t2 t3))
TimesZeroL §§ (Times Zero _) = pure Zero
TimesZeroR §§ _ = Nothing -- We can't pull a type out of thin air
Distribute §§ (Times (Plus t1 t2) t3) = pure (Plus (Times t1 t3) (Times t2 t3))
Factor §§ (Plus (Times t1 t3) (Times t2 t3'))
  | t3 == t3' = pure (Times (Plus t1 t2) t3)
(§§) _ _ = empty

-- | Semantics of combinators.
(@@) :: Iso -> Val -> Maybe Val
Id @@ v = pure v
(Sym f) @@ v = adjoint f @@ v
(f :.: g) @@ v = (f @@ v) >>= (@@) g
(f :*: g) @@ (Prod v1 v2) = Prod <$> (f @@ v1) <*> (g @@ v2)
(f :+: _) @@ (SumL v1) = SumL <$> (f @@ v1)
(_ :+: g) @@ (SumR v2) = SumR <$> (g @@ v2)
PlusZeroL @@ (SumR v1) = pure v1
PlusZeroR @@ v1 = pure (SumR v1)
CommutePlus @@ (SumL v1) = pure (SumR v1)
CommutePlus @@ (SumR v2) = pure (SumL v2)
AssocPlusL @@ (SumL v1) = pure (SumL (SumL v1))
AssocPlusL @@ (SumR (SumL v2)) = pure (SumL (SumR v2))
AssocPlusL @@ (SumR (SumR v3)) = pure (SumR v3)
AssocPlusR @@ (SumL (SumL v1)) = pure (SumL v1)
AssocPlusR @@ (SumL (SumR v2)) = pure (SumR (SumL v2))
AssocPlusR @@ (SumR v3) = pure (SumR (SumR v3))
TimesOneL @@ (Prod Unit v1) = pure v1
TimesOneR @@ v1 = pure (Prod Unit v1)
CommuteTimes @@ (Prod v1 v2) = pure (Prod v2 v1)
AssocTimesL @@ (Prod v1 (Prod v2 v3)) = pure (Prod (Prod v1 v2) v3)
AssocTimesR @@ (Prod (Prod v1 v2) v3) = pure (Prod v1 (Prod v2 v3))
Distribute @@ (Prod (SumL v2) v1) = pure (SumL (Prod v2 v1))
Distribute @@ (Prod (SumR v3) v1) = pure (SumR (Prod v3 v1))
Factor @@ (SumL (Prod v2 v1)) = pure (Prod (SumL v2) v1)
Factor @@ (SumR (Prod v3 v1)) = pure (Prod (SumR v3) v1)
(@@) _ _ = empty

-- | The size of a type is the number of unique
-- values that can inhabit it.
size :: (Num a) => Typ -> a
size Zero = 0
size One = 1
size (Plus t1 t2) = size t1 + size t2
size (Times t1 t2) = size t1 * size t2

-- | A simple type is either the zero type,
-- or a right-associative sum of ones.
simpleTyp :: Typ -> Bool
simpleTyp Zero = True
simpleTyp typ = simpleTyp' typ
  where
    simpleTyp' One = True
    simpleTyp' (Plus One t) = simpleTyp' t
    simpleTyp' _ = False

-- | Given a type `t`, we can (perhaps) find an isomorphism which turns
-- any value of type `t` into a value `v'` of semi-simple type `t'`.
--
-- The only times this function should return `Nothing` is when the type
-- has size 0, in which case no value of this type can exist anyway.
simplify :: Typ -> Maybe Iso
simplify typ =
  case typ of
    One -> pure Id
    Zero -> pure Id -- sus, might loop forever.
    -- Common simplification patterns.
    -- All of these leave the type in a "simpler" state,
    -- and so even when recursing with `(:+:)` and `(:*:)`
    -- the recursion should always eventually terminate.
    --
    -- What do we mean by a simpler state? It is one which has either:
    --   1. Fewer terms (done by `PlusZeroL`, `TimesZeroL` and `TimesOneL`)
    --   2. Multiplication pushed down (done by `Distribute`)
    --   3. Addition associated more to the right (done by `AssocPlusR`)
    --
    -- The idea is that once multiplication has been pushed all the way down,
    -- we can use `PlusZeroL` to remove superflous 1s, and associate the remaining
    -- addition to the right.
    Plus Zero t ->
      let c = PlusZeroL
       in loop c t
    Plus t Zero ->
      let c = CommutePlus :.: PlusZeroL
       in loop c t
    Plus (Plus t1 t2) t3 ->
      let c = AssocPlusR
          t = Plus t1 (Plus t2 t3)
       in loop c t
    Times Zero _ ->
      let c = TimesZeroL
          t = Zero
       in loop c t
    Times _ Zero ->
      let c = CommuteTimes :.: TimesZeroL
          t = Zero
       in loop c t
    Times One t ->
      let c = TimesOneL
       in loop c t
    Times t One ->
      let c = CommuteTimes :.: TimesOneL
       in loop c t
    Times (Plus t1 t2) t3 ->
      let c = Distribute
          t = Plus (Times t1 t3) (Times t2 t3)
       in loop c t
    Times t3 (Plus t1 t2) ->
      let c = CommuteTimes :.: Distribute
          t = Plus (Times t1 t3) (Times t2 t3)
       in loop c t
    -- Otherwise recurse into type and hope it terminates.
    Plus t1 t2 -> do
      c <- (:+:) <$> simplify t1 <*> simplify t2
      t <- c §§ Plus t1 t2
      loop c t
    Times t1 t2 -> do
      c <- (:*:) <$> simplify t1 <*> simplify t2
      t <- c §§ Times t1 t2
      loop c t
  where
    loop c t = do
      let assertion = assert (c §§ typ == Just t)
          c' = optimize c
      assertion (if simpleTyp t then pure c' else (c' :.:) <$> simplify t)
    optimize (Id :.: c) = c
    optimize (c :.: Id) = c
    optimize (Id :+: Id) = Id
    optimize (Id :*: Id) = Id
    optimize c = c

-- | Get a list of all possible values that can inhabit this type.
enumerate :: Typ -> [Val]
enumerate typ =
  case typ of
    Zero -> []
    One -> pure Unit
    Times t1 t2 -> do
      v1 <- enumerate t1
      v2 <- enumerate t2
      pure $ Prod v1 v2
    Plus t1 t2 -> map SumL (enumerate t1) <> map SumR (enumerate t2)
