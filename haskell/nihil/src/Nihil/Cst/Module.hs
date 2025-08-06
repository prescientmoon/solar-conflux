module Nihil.Cst.Module
  ( Module (..)
  , EOF (..)
  , Declaration (..)
  , IndLikeKind (..)
  , IndLike (..)
  , Field (..)
  , TypeAlias (..)
  , ForeignType (..)
  , ForeignValue (..)
  , ValueTypeAnnotation (..)
  , ValueEquation (..)
  ) where

import Relude

import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Expr qualified as Expr
import Nihil.Cst.Type qualified as Type
import Optics qualified as O
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M

data Module
  = Module
  { module' ∷ Maybe Base.Token'
  , name ∷ Maybe Base.Name
  , exports ∷ Maybe (Base.Delimited (Base.Separated Base.Token' Base.Name))
  , where' ∷ Maybe Base.Token'
  , decls ∷ Seq Declaration
  , eof ∷ Base.Token EOF
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Module where
  pretty (Module{..}) =
    Base.prettyTree
      "Module"
      $ catMaybes
        [ PP.pretty <$> module'
        , PP.pretty <$> name
        , PP.pretty <$> exports
        , PP.pretty <$> where'
        , guard (not $ null decls) $> Base.prettyTree "declarations" do
            PP.pretty <$> toList decls
        , Just $ PP.pretty eof
        ]

data EOF = EOF
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty EOF where
  pretty EOF = "EOF"

data Declaration
  = DeclIndLike IndLike
  | DeclTypeAlias TypeAlias
  | DeclForeignType ForeignType
  | DeclForeignValue ForeignValue
  | DeclValueTypeAnn ValueTypeAnnotation
  | DeclValueEquation ValueEquation
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Declaration where
  pretty (DeclIndLike i) = PP.pretty i
  pretty (DeclTypeAlias i) = PP.pretty i
  pretty (DeclForeignType i) = PP.pretty i
  pretty (DeclForeignValue i) = PP.pretty i
  pretty (DeclValueTypeAnn i) = PP.pretty i
  pretty (DeclValueEquation i) = PP.pretty i

-- attachTrivia t (DeclIndLike i) = DeclIndLike <$> Base.attachTrivia t i
-- attachTrivia t (DeclTypeAlias i) = DeclTypeAlias <$> Base.attachTrivia t i
-- attachTrivia t (DeclForeignType i) = DeclForeignType <$> Base.attachTrivia t i
-- attachTrivia t (DeclForeignValue i) = DeclForeignValue <$> Base.attachTrivia t i
-- attachTrivia t (DeclValue i) = DeclValue <$> Base.attachTrivia t i

data IndLikeKind = Inductive | Coinductive | Trait
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty IndLikeKind where
  pretty Inductive = "inductive"
  pretty Coinductive = "coinductive"
  pretty Trait = "trait"

data IndLike
  = IndLike
  { kind ∷ Base.Token IndLikeKind
  , name ∷ Maybe Base.Name
  , args ∷ Seq Base.Name
  , where' ∷ Maybe Base.Token'
  , fields ∷ Seq Field
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty IndLike where
  pretty (IndLike{..}) =
    Base.prettyTree (show $ O.view #value kind) $
      catMaybes
        [ Just $ PP.pretty kind
        , PP.pretty <$> name
        , guard (not $ null args) $> Base.prettyTree "arguments" do
            toList $ PP.pretty <$> args
        , PP.pretty <$> where'
        , guard (not $ null fields) $> Base.prettyTree "fields" do
            toList $ PP.pretty <$> fields
        ]

data Field
  = Field
  { start ∷ M.SourcePos
  -- ^ In practice, at least one of the field's props is always @Nothing@
  -- (otherwise we would fail the parser), although the type system doesn't
  -- know this, hence we provide this additional start position such that
  -- @Base.HasSpan@ can be implemented totally.
  , name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Field where
  pretty (Field{..}) =
    Base.prettyTree "Field" . catMaybes $
      [ PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data TypeAlias
  = TypeAlias
  { ty ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , args ∷ Seq Base.Name
  , eq ∷ Maybe Base.Token'
  , body ∷ Maybe Type.Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty TypeAlias where
  pretty (TypeAlias{..}) =
    Base.prettyTree "Type alias" . catMaybes $
      [ Just $ PP.pretty ty
      , PP.pretty <$> name
      , guard (not $ null args) $> Base.prettyTree "arguments" do
          toList $ PP.pretty <$> args
      , PP.pretty <$> eq
      , PP.pretty <$> body
      ]

data ForeignType
  = ForeignType
  { foreign' ∷ Base.Token'
  , type' ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty ForeignType where
  pretty (ForeignType{..}) =
    Base.prettyTree "Foreign type" . catMaybes $
      [ Just $ PP.pretty foreign'
      , Just $ PP.pretty type'
      , PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data ForeignValue
  = ForeignValue
  { foreign' ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty ForeignValue where
  pretty (ForeignValue{..}) =
    Base.prettyTree "Foreign value" . catMaybes $
      [ Just $ PP.pretty foreign'
      , PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data ValueTypeAnnotation
  = ValueTypeAnnotation
  { name ∷ Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty ValueTypeAnnotation where
  pretty (ValueTypeAnnotation{..}) =
    Base.prettyTree "Value type annotation" . catMaybes $
      [ Just $ PP.pretty name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data ValueEquation = ValueEquation
  { name ∷ Base.Name
  , args ∷ Seq Expr.Pattern
  , eq ∷ Maybe Base.Token'
  , expr ∷ Maybe Expr.Expr
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty ValueEquation where
  pretty (ValueEquation{..}) =
    Base.prettyTree "Value equation" . catMaybes $
      [ Just $ PP.pretty name
      , guard (not $ null args) $> Base.prettyTree "arguments" do
          toList $ PP.pretty <$> args
      , PP.pretty <$> eq
      , PP.pretty <$> expr
      ]
