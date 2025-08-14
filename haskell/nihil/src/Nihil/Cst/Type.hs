module Nihil.Cst.Type
  ( Type' (..)
  , Forall (..)
  , ArrowKind (..)
  , Arrow (..)
  , App (..)
  , Var (..)
  ) where

import Relude

import Nihil.Cst.Base qualified as Base
import Prettyprinter qualified as PP

data Type'
  = TyForall Forall
  | TyArrow Arrow
  | TyApp App
  | TyVar Var
  | TyParens (Base.Delimited (Maybe Type'))
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Type' where
  pretty (TyForall x) = PP.pretty x
  pretty (TyVar x) = PP.pretty x
  pretty (TyParens x) = PP.pretty x
  pretty (TyApp x) = PP.pretty x
  pretty (TyArrow x) = PP.pretty x

data Forall = Forall
  { tForall ∷ Base.Token'
  , names ∷ Seq Base.Name
  , comma ∷ Maybe Base.Token'
  , ty ∷ Maybe Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Forall where
  pretty (Forall{..}) =
    Base.prettyTree "Forall" . catMaybes $
      [ Just $ PP.pretty tForall
      , guard (not $ null names) $> Base.prettyTree "type vars" do
          PP.pretty <$> toList names
      , PP.pretty <$> comma
      , PP.pretty <$> ty
      ]

data ArrowKind = Normal | Trait
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty ArrowKind where
  pretty Normal = "->"
  pretty Trait = "=>"

data Arrow = Arrow
  { from ∷ Maybe Type'
  , kind ∷ Base.Token ArrowKind
  , to ∷ Maybe Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Arrow where
  pretty (Arrow{..}) =
    Base.prettyTree "Arrow" . catMaybes $
      [ PP.pretty <$> from
      , Just $ PP.pretty kind
      , PP.pretty <$> to
      ]

data App = App
  { f ∷ Type'
  , a ∷ Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty App where
  pretty (App{..}) =
    Base.prettyTree
      "App"
      [ PP.pretty f
      , PP.pretty a
      ]

data Var = Var
  { name ∷ Base.Name
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Var where
  pretty (Var{..}) = PP.pretty name
