module Nihil.Cst.Type
  ( Type' (..)
  , Forall (..)
  , ArrowKind (..)
  , Arrow (..)
  , App (..)
  , Ref (..)
  , Var (..)
  ) where

import Relude

import Nihil.Cst.Base qualified as Base

data Type'
  = TyForall Forall
  | TyArrow Arrow
  | TyApp App
  | TyRef Ref
  | TyVar Var
  | TyParens (Base.Delimited Type')
  deriving (Generic, Show)

data Forall = Forall
  { tForall ∷ Base.Token ()
  , names ∷ [Base.Name]
  , dot ∷ Maybe (Base.Token ())
  , ty ∷ Maybe Type'
  }
  deriving (Generic, Show)

data ArrowKind = Normal | Trait | Self
  deriving (Generic, Show)

data Arrow = Arrow
  { from ∷ Maybe Type'
  , kind ∷ ArrowKind
  , to ∷ Maybe Type'
  }
  deriving (Generic, Show)

data App = App
  { f ∷ Type'
  , a ∷ Type'
  }
  deriving (Generic, Show)

data Ref = Ref
  { ampersand ∷ Base.Token ()
  , ty ∷ Maybe Type'
  }
  deriving (Generic, Show)

data Var = Var
  { name ∷ Base.Name
  }
  deriving (Generic, Show)
