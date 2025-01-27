{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic definitions used by many parts of the interpreter.
--
-- We use the same name types as in Futhark:
-- https://hackage.haskell.org/package/futhark-0.24.3/docs/Language-Futhark-Core.html#t:Name
module QTheseus.Core where

import Data.String (IsString)
import qualified Data.Text as T

-- | An abstract type representing names.
newtype Name = Name T.Text
  deriving (Show, Eq, Ord, IsString, Semigroup)

nameToString :: Name -> String
nameToString (Name t) = T.unpack t

nameFromString :: String -> Name
nameFromString = Name . T.pack

nameToText :: Name -> T.Text
nameToText (Name t) = t

nameFromText :: T.Text -> Name
nameFromText = Name

-- | A name tagged with an integer,
-- which is used for comparisons
-- allowing distinct names with the same text.
data VName = VName Name Integer
  deriving (Show)

baseTag :: VName -> Integer
baseTag (VName _ tag) = tag

baseName :: VName -> Name
baseName (VName name _) = name

instance Eq VName where
  VName _ x == VName _ y = x == y

instance Ord VName where
  VName _ x `compare` VName _ y = x `compare` y
