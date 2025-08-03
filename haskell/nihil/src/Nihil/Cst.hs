module Nihil.Cst () where

import Nihil.Cst.Expr (Expr)
import Nihil.Cst.Module (Declaration)
import Nihil.Cst.Type (Type')

data UntypedCst
  = CType Type'
  | CExpr Expr
  | CDecl Declaration
