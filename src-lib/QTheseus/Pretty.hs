{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pretty printing for the QTheseus syntax.
module QTheseus.Pretty (renderText, prettyProg) where

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import QTheseus.Core
import QTheseus.Syntax

renderText :: Doc ann -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions

prettyProg :: (Pretty vn) => ProgBase vn -> Doc ann
prettyProg prog = vcat ((<> line) . pretty <$> prog)

instance (Pretty vn) => Pretty (Def vn) where
  pretty (DataTyp name constrs) =
    let cs = pretty <$> constrs
        seps = ["=", "|"] ++ tail seps
        tname = "type" <+> pretty name
        targs = vcat $ zipWith (<+>) seps cs
     in vcat [tname, indent 2 targs]
  pretty (Iso fn t cs) =
    let decl = "iso" <+> pretty fn <+> ":" <+> pretty t
        body = vcat (("|" <+>) . pretty <$> cs)
     in vcat [decl, body]
  pretty (Eval fn x) = "eval" <+> pretty fn <+> pretty x

instance (Pretty vn) => Pretty (Typ vn) where
  pretty One = "1"
  pretty Zero = "0"
  pretty (Times t1 t2) = parens $ pretty t1 <+> "*" <+> pretty t2
  pretty (Plus t1 t2) = parens $ pretty t1 <+> "+" <+> pretty t2
  pretty (TypDef name) = pretty name
  pretty (TypVar name) = "'" <> pretty name

instance (Pretty vn) => Pretty (PVal vn) where
  pretty Unit = "()"
  pretty (Prod t1 t2) = parens $ pretty t1 <+> "," <+> pretty t2
  pretty (SumL t) = "inL" <+> pretty t
  pretty (SumR t) = "inR" <+> pretty t
  pretty (Var v) = pretty v
  pretty (Constr c v) = pretty c <+> pretty v
  pretty (App f x) = pretty f <+> pretty x

instance (Pretty vn) => Pretty (Constructor vn) where
  pretty (Constructor name typ) = pretty name <+> pretty typ

instance (Pretty vn) => Pretty (ITyp vn) where
  pretty (ITyp t1 t2) = pretty t1 <+> "<->" <+> pretty t2

instance (Pretty vn) => Pretty (Clause vn) where
  pretty (Clause x1 x2) = pretty x1 <+> "<->" <+> pretty x2

instance Pretty Name where
  pretty (Name name) = pretty name

instance Pretty VName where
  pretty (VName name tag) = pretty name <> "_" <> pretty tag
