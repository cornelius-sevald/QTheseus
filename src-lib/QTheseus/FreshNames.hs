{-# LANGUAGE FlexibleInstances #-}

-- | Facilities for generating new names.
-- Ripped directly from:
-- https://hackage.haskell.org/package/futhark-0.24.3/docs/Futhark-FreshNames.html
-- and
-- https://hackage.haskell.org/package/futhark-0.24.3/docs/Futhark-MonadFreshNames.html
module QTheseus.FreshNames
  ( VNameSource,
    blankNameSource,
    newNameSource,
    newVName,
    MonadFreshNames (..),
    newVNameM,
    newNameM,
  )
where

import qualified Control.Monad.RWS
import qualified Control.Monad.State
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer
import QTheseus.Core

newtype VNameSource = VNameSource Integer
  deriving (Eq, Ord)

instance Semigroup VNameSource where
  VNameSource x <> VNameSource y = VNameSource (max x y)

instance Monoid VNameSource where
  mempty = blankNameSource

-- | Produce a fresh `VName`, given a `VName` as a template.
newVName :: VNameSource -> VName -> (VName, VNameSource)
newVName vns k = newName vns (baseName k)

-- | Produce a fresh `VName`, given a `Name` as a template.
newName :: VNameSource -> Name -> (VName, VNameSource)
newName (VNameSource i) k = (VName k i, VNameSource (succ i))

blankNameSource :: VNameSource
blankNameSource = newNameSource 0

newNameSource :: Integer -> VNameSource
newNameSource = VNameSource

{- Monad instance for fresh names. -}

class (Monad m) => MonadFreshNames m where
  getNameSource :: m VNameSource
  putNameSource :: VNameSource -> m ()

-- | Run a computation needing a fresh name source and returning a new
-- one, using 'getNameSource' and 'putNameSource' before and after the
-- computation.
modifyNameSource :: (MonadFreshNames m) => (VNameSource -> (a, VNameSource)) -> m a
modifyNameSource f = do
  src <- getNameSource
  let (x, src') = f src
  putNameSource src'
  pure x

-- | Produce a fresh `VName`, given a `VName` as a template.
newVNameM :: (MonadFreshNames m) => VName -> m VName
newVNameM = modifyNameSource . flip newVName

-- | Produce a fresh `VName`, given a `Name` as a template.
newNameM :: (MonadFreshNames m) => Name -> m VName
newNameM = modifyNameSource . flip newName

instance (Monad im) => MonadFreshNames (Control.Monad.State.StateT VNameSource im) where
  getNameSource = Control.Monad.State.get
  putNameSource = Control.Monad.State.put

instance
  (Monad im, Monoid w) =>
  MonadFreshNames (Control.Monad.RWS.RWST r w VNameSource im)
  where
  getNameSource = Control.Monad.RWS.get
  putNameSource = Control.Monad.RWS.put

-- Utility instance defintions for MTL classes.  This requires
-- UndecidableInstances, but saves on typing elsewhere.

instance (MonadFreshNames m) => MonadFreshNames (Control.Monad.Trans.Reader.ReaderT s m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m, Monoid s) =>
  MonadFreshNames (Control.Monad.Trans.Writer.WriterT s m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m) =>
  MonadFreshNames (Control.Monad.Trans.Maybe.MaybeT m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m) =>
  MonadFreshNames (Control.Monad.Trans.Except.ExceptT e m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource
