module Nihil.Cst.Module
  ( Module (..)
  , Declaration (..)
  , IndLikeKind (..)
  , IndLike (..)
  , Field (..)
  , TypeAlias (..)
  , ForeignType (..)
  , ForeignValue (..)
  , Value (..)
  , ValueEquation (..)
  ) where

import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Expr qualified as Expr
import Nihil.Cst.Type qualified as Type

data Module
  = Module
  { module' ∷ Maybe (Base.Token ())
  , name ∷ Maybe Base.Name
  , exports ∷ Maybe (Base.Delimited Base.Name)
  , where' ∷ Maybe (Base.Token ())
  , decls ∷ [Declaration]
  , eof ∷ Base.Token ()
  }
  deriving (Generic, Show)

data Declaration
  = DeclIndLike IndLike
  | DeclTypeAlias TypeAlias
  | DeclForeignType ForeignType
  | DeclForeignValue ForeignValue
  | DeclValue Value
  deriving (Generic, Show)

data IndLikeKind = Inductive | Coinductive | Trait
  deriving (Generic, Show)

data IndLike
  = IndLike
  { kind ∷ Base.Token IndLikeKind
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  , where' ∷ Maybe (Base.Token ())
  , fields ∷ [Field]
  }
  deriving (Generic, Show)

data Field
  = Field
  { name ∷ Maybe Base.Name
  , colon ∷ Maybe (Base.Token ())
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show)

data TypeAlias
  = TypeAlias
  { ty ∷ Base.Token ()
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  , eq ∷ Maybe (Base.Token ())
  , body ∷ Maybe (Type.Type')
  }
  deriving (Generic, Show)

data ForeignType
  = ForeignType
  { foreign' ∷ Base.Token ()
  , ty ∷ Base.Token ()
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  }
  deriving (Generic, Show)

data ForeignValue
  = ForeignValue
  { foreign' ∷ Base.Token ()
  , name ∷ Maybe Base.Name
  , colon ∷ Maybe (Base.Token ())
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show)

data Value
  = Value
  { name ∷ Maybe Base.Name
  , colon ∷ Maybe (Base.Token ())
  , ty ∷ Maybe Type.Type'
  , branches ∷ [ValueEquation]
  }
  deriving (Generic, Show)

data ValueEquation = ValueEquation
  { name ∷ Base.Name
  , args ∷ [Expr.Pattern]
  , eq ∷ Maybe (Base.Token ())
  , expr ∷ Maybe Expr.Expr
  }
  deriving (Generic, Show)
