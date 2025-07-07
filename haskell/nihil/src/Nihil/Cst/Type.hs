module Nihil.Cst.Type
  ( Type' (..)
  , Forall (..)
  , ArrowKind (..)
  , Arrow (..)
  , App (..)
  , Var (..)
  ) where

import Relude

import Data.Foldable1 (foldl1)
import Nihil.Cst.Base qualified as Base
import Nihil.Error qualified as Error
import Optics qualified as O
import Prettyprinter qualified as PP

data Type'
  = TyForall Forall
  | TyArrow Arrow
  | TyApp App
  | TyVar Var
  | TyParens (Base.Delimited (Maybe Type'))
  deriving (Generic, Show)

instance PP.Pretty Type' where
  pretty (TyForall x) = PP.pretty x
  pretty (TyVar x) = PP.pretty x
  pretty (TyParens x) = PP.pretty x
  pretty (TyApp x) = PP.pretty x
  pretty (TyArrow x) = PP.pretty x

instance Base.HasSpan Type' where
  spanOf (TyForall x) = Base.spanOf x
  spanOf (TyVar x) = Base.spanOf x
  spanOf (TyArrow x) = Base.spanOf x
  spanOf (TyApp x) = Base.spanOf x
  spanOf (TyParens (Base.Delimited open inner close)) =
    Base.mergeSpans
      (Base.spanOf open)
      [ Base.spanOf <$> inner
      , Base.spanOf <$> close
      ]

instance Base.HasTrivia Type' where
  attachTrivia t (TyForall x) = TyForall <$> Base.attachTrivia t x
  attachTrivia t (TyVar x) = TyVar <$> Base.attachTrivia t x
  attachTrivia t (TyArrow x) = TyArrow <$> Base.attachTrivia t x
  attachTrivia t (TyApp x) = TyApp <$> Base.attachTrivia t x
  attachTrivia t (TyParens x) = TyParens <$> Base.attachTrivia t x

data Forall = Forall
  { tForall ∷ Base.Token'
  , names ∷ Seq Base.Name
  , comma ∷ Maybe Base.Token'
  , ty ∷ Maybe Type'
  }
  deriving (Generic, Show)

instance PP.Pretty Forall where
  pretty (Forall{..}) =
    Base.prettyTree "Forall" . catMaybes $
      [ Just $ PP.pretty tForall
      , guard (not $ null names) $> Base.prettyTree "type vars" do
          PP.pretty <$> toList names
      , PP.pretty <$> comma
      , PP.pretty <$> ty
      ]

instance Base.HasSpan Forall where
  spanOf (Forall{..}) =
    Base.mergeSpans
      (Base.spanOf tForall)
      [ fmap (foldl1 Error.mergeSpans)
          . nonEmpty
          . fmap Base.spanOf
          $ toList names
      , Base.spanOf <$> comma
      , Base.spanOf <$> ty
      ]

instance Base.HasTrivia Forall where
  attachTrivia t = Just . O.over #tForall (Base.tokAttachTrivia t)

data ArrowKind = Normal | Trait
  deriving (Generic, Show)

instance PP.Pretty ArrowKind where
  pretty Normal = "->"
  pretty Trait = "=>"

data Arrow = Arrow
  { from ∷ Maybe Type'
  , kind ∷ Base.Token ArrowKind
  , to ∷ Maybe Type'
  }
  deriving (Generic, Show)

instance PP.Pretty Arrow where
  pretty (Arrow{..}) =
    Base.prettyTree "Arrow" . catMaybes $
      [ PP.pretty <$> from
      , Just $ PP.pretty kind
      , PP.pretty <$> to
      ]

instance Base.HasSpan Arrow where
  spanOf (Arrow{..}) =
    Base.mergeSpans
      (Base.spanOf kind)
      [ Base.spanOf <$> from
      , Base.spanOf <$> to
      ]

instance Base.HasTrivia Arrow where
  attachTrivia t a@(Arrow{..}) = case Base.attachTrivia t from of
    Nothing → Just $ O.over #kind (Base.tokAttachTrivia t) a
    Just t' → Just $ O.set #from t' a

data App = App
  { f ∷ Type'
  , a ∷ Type'
  }
  deriving (Generic, Show)

instance Base.HasTrivia App where
  attachTrivia t a =
    Base.attachTriviaOptically #f t a
      <|> Base.attachTriviaOptically #a t a

instance PP.Pretty App where
  pretty (App{..}) =
    Base.prettyTree
      "App"
      [ PP.pretty f
      , PP.pretty a
      ]

instance Base.HasSpan App where
  spanOf (App{..}) = Error.mergeSpans (Base.spanOf f) (Base.spanOf a)

data Var = Var
  { name ∷ Base.Name
  }
  deriving (Generic, Show)

instance PP.Pretty Var where
  pretty (Var{..}) = PP.pretty name

instance Base.HasSpan Var where
  spanOf (Var{..}) = Base.spanOf name

instance Base.HasTrivia Var where
  attachTrivia t = Just . O.over #name (Base.tokAttachTrivia t)
