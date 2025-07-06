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

import Relude

import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Expr qualified as Expr
import Nihil.Cst.Type qualified as Type
import Prettyprinter qualified as PP

data Module
  = Module
  { module' ∷ Maybe Base.Token'
  , name ∷ Maybe Base.Name
  , exports ∷ Maybe (Base.Delimited (Base.Separated Base.Token' Base.Name))
  , where' ∷ Maybe Base.Token'
  , decls ∷ Seq Declaration
  , eof ∷ Base.Token ()
  }
  deriving (Generic, Show)

instance PP.Pretty Module where
  pretty (Module{..}) =
    Base.prettyTree
      "module"
      $ catMaybes
        [ PP.pretty <$> module'
        , PP.pretty <$> name
        , PP.pretty <$> exports
        , PP.pretty <$> where'
        , guard (not $ null decls) $> Base.prettyTree "declarations" do
            PP.pretty <$> toList decls
        , Just $ PP.pretty eof
        ]

data Declaration
  = DeclIndLike IndLike
  | DeclTypeAlias TypeAlias
  | DeclForeignType ForeignType
  | DeclForeignValue ForeignValue
  | DeclValue Value
  deriving (Generic, Show)

instance PP.Pretty Declaration where
  pretty (DeclIndLike i) = PP.pretty i
  pretty (DeclTypeAlias i) = PP.pretty i
  pretty (DeclForeignType i) = PP.pretty i
  pretty (DeclForeignValue i) = PP.pretty i
  pretty (DeclValue i) = PP.pretty i

instance Base.HasTrivia Declaration where
  attachTrivia _ _ = Nothing

-- attachTrivia t (DeclIndLike i) = DeclIndLike <$> Base.attachTrivia t i
-- attachTrivia t (DeclTypeAlias i) = DeclTypeAlias <$> Base.attachTrivia t i
-- attachTrivia t (DeclForeignType i) = DeclForeignType <$> Base.attachTrivia t i
-- attachTrivia t (DeclForeignValue i) = DeclForeignValue <$> Base.attachTrivia t i
-- attachTrivia t (DeclValue i) = DeclValue <$> Base.attachTrivia t i

data IndLikeKind = Inductive | Coinductive | Trait
  deriving (Generic, Show)

instance PP.Pretty IndLikeKind where
  pretty Inductive = "inductive"
  pretty Coinductive = "coinductive"
  pretty Trait = "trait"

data IndLike
  = IndLike
  { kind ∷ Base.Token IndLikeKind
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  , where' ∷ Maybe Base.Token'
  , fields ∷ [Field]
  }
  deriving (Generic, Show)

instance PP.Pretty IndLike where
  pretty (IndLike{..}) =
    Base.prettyTree (PP.pretty kind) $
      catMaybes
        [ PP.pretty <$> name
        , guard (not $ null args) $> Base.prettyTree "arguments" do
            PP.pretty <$> args
        , PP.pretty <$> where'
        , guard (not $ null fields) $> Base.prettyTree "fields" do
            PP.pretty <$> fields
        ]

data Field
  = Field
  { name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show)

instance PP.Pretty Field where
  pretty (Field{..}) =
    Base.prettyTree "field" . catMaybes $
      [ PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data TypeAlias
  = TypeAlias
  { ty ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  , eq ∷ Maybe Base.Token'
  , body ∷ Maybe Type.Type'
  }
  deriving (Generic, Show)

instance PP.Pretty TypeAlias where
  pretty (TypeAlias{..}) =
    Base.prettyTree "type alias" . catMaybes $
      [ Just $ PP.pretty ty
      , PP.pretty <$> name
      , guard (not $ null args) $> Base.prettyTree "arguments" do
          PP.pretty <$> args
      , PP.pretty <$> eq
      , PP.pretty <$> body
      ]

data ForeignType
  = ForeignType
  { foreign' ∷ Base.Token'
  , ty ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , args ∷ [Base.Name]
  }
  deriving (Generic, Show)

instance PP.Pretty ForeignType where
  pretty (ForeignType{..}) =
    Base.prettyTree "foreign type" . catMaybes $
      [ Just $ PP.pretty foreign'
      , Just $ PP.pretty ty
      , PP.pretty <$> name
      , guard (not $ null args) $> Base.prettyTree "arguments" do
          PP.pretty <$> args
      ]

data ForeignValue
  = ForeignValue
  { foreign' ∷ Base.Token'
  , name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  }
  deriving (Generic, Show)

instance PP.Pretty ForeignValue where
  pretty (ForeignValue{..}) =
    Base.prettyTree "foreign value" . catMaybes $
      [ Just $ PP.pretty foreign'
      , PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      ]

data Value
  = Value
  { name ∷ Maybe Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  , branches ∷ [ValueEquation]
  }
  deriving (Generic, Show)

instance PP.Pretty Value where
  pretty (Value{..}) =
    Base.prettyTree "value" . catMaybes $
      [ PP.pretty <$> name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      , guard (not $ null branches) $> Base.prettyTree "branches" do
          PP.pretty <$> branches
      ]

data ValueEquation = ValueEquation
  { name ∷ Base.Name
  , args ∷ [Expr.Pattern]
  , eq ∷ Maybe Base.Token'
  , expr ∷ Maybe Expr.Expr
  }
  deriving (Generic, Show)

instance PP.Pretty ValueEquation where
  pretty (ValueEquation{..}) =
    Base.prettyTree "value equation" . catMaybes $
      [ Just $ PP.pretty name
      , guard (not $ null args) $> Base.prettyTree "arguments" do
          PP.pretty <$> args
      , PP.pretty <$> eq
      , PP.pretty <$> expr
      ]
