module Nihil.Ast.Type where

import Nihil.Cst.Base (Name)
import Nihil.Cst.Type qualified as Cst
import Nihil.Error (Span)
import Relude

data Type'
  = TyForall Span Name Type'
  | TyArrow Span Cst.ArrowKind Type' Type'
  | TyApp Span Type' Type'
  | TyVar Span Name
  | TyUnknown Span
  deriving (Generic, Show)
