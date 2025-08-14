module Nihil.Compiler.Ast
  ( Name (..)
  , Binder
  , nameToText
  , forceMaybeName
  , forceMaybeUsed
  , ScopeId
  , NodeId
  , IsNode
  , nodeId
  , Scope (..)
  , atExprInScope
  , atTypeInScope
  , Type' (..)
  , Expr (..)
  , Pattern (..)
  , Definition (..)
  , TypeDefinition (..)
  , ExprDefinition (..)
  ) where

import Data.HashMap.Strict qualified as HashMap
import Nihil.Cst.Base qualified as Cst
import Nihil.Cst.Type qualified as Cst.Type
import Nihil.Utils (Icit)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- Building blocks
-- {{{ Names & binders
data Name
  = Unresolved NodeId Text
  | Resolved NodeId ScopeId Text
  deriving (Show, Generic, Eq, Hashable)

type Binder = Name

nameToText ∷ Name → Text
nameToText = \case
  Unresolved _ n → n
  Resolved _ s n → "[:" <> show s <> "]." <> n

-- | The parser often returns @Maybe Cst.Name@, which this function forcefully
-- turns into a piece of text.
forceMaybeName ∷ NodeId → Maybe Cst.Name → Text
forceMaybeName (NodeId i) Nothing = "__unnammed_" <> show i
forceMaybeName _ (Just t) = t.value

-- | Some names are already used, but we still want to keep track of the declarations.
--
-- Thus, we invent a new name if that's the case
forceMaybeUsed ∷ NodeId → Bool → Text → Text
forceMaybeUsed _ True x = x
forceMaybeUsed (NodeId i) False x = "__overload" <> show i <> x

-- }}}
-- {{{ Scopes
newtype ScopeId = ScopeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

-- }}}
-- {{{ Nodes
newtype NodeId = NodeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

class IsNode a where
  nodeId ∷ a → NodeId

instance IsNode NodeId where
  nodeId = id

instance IsNode Name where
  nodeId (Unresolved i _) = i
  nodeId (Resolved i _ _) = i

-- }}}

-- AST types
-- {{{ Definitions
data TypeDefinition
  = TForeign NodeId Type' -- Kind
  | TAlias NodeId Type'
  | TParam NodeId
  deriving (Show, Generic)

data ExprDefinition
  = EForeign NodeId Type'
  | EDeclaration NodeId Type' Expr
  | EParam NodeId
  deriving (Show, Generic)

data Definition a
  = Definition
  { names ∷ NonEmpty NodeId
  -- ^ Can be used to get the span of the name at the definition site.
  , value ∷ a
  }
  deriving (Show, Generic)

-- }}}
-- {{{ Scopes
data Scope = Scope
  { inherits ∷ Seq ScopeId
  , types ∷ HashMap Text (Definition TypeDefinition)
  , exprs ∷ HashMap Text (Definition ExprDefinition)
  , scopes ∷ HashMap Text ScopeId
  }
  deriving (Generic)

instance Semigroup Scope where
  a <> b =
    Scope
      { inherits = a.inherits <> b.inherits
      , types = a.types <> b.types
      , exprs = a.exprs <> b.exprs
      , scopes = a.scopes <> b.scopes
      }

instance Monoid Scope where
  mempty = Scope mempty mempty mempty mempty

atExprInScope ∷ Text → O.Lens' Scope (Maybe (Definition ExprDefinition))
atExprInScope name = #exprs % O.at name

atTypeInScope ∷ Text → O.Lens' Scope (Maybe (Definition TypeDefinition))
atTypeInScope name = #types % O.at name

-- }}}
-- {{{ Types
data Type'
  = TyForall NodeId ScopeId Binder Type'
  | TyArrow NodeId Cst.Type.ArrowKind Type' Type'
  | TyVar NodeId Name
  | TyApp NodeId Type' Type'
  | TyLambda NodeId ScopeId Binder Type'
  | TyUnknown NodeId
  deriving (Generic, Show)

-- }}}
-- {{{ Expressions
data Expr
  = EVar NodeId Name
  | EApp NodeId Expr Expr
  | EMatch NodeId (Seq Expr) (Seq (Seq Pattern, ScopeId, Expr))
  | EUnknown NodeId
  | EPi NodeId ScopeId Icit Binder Expr Expr
  | EAnnotation NodeId Expr Expr
  | EHole NodeId
  deriving (Generic, Show)

data Pattern
  = PName NodeId Binder
  | PProj NodeId Name (Seq Pattern) -- (Thing a b) → ...
  | PWildcard NodeId -- _ → ...
  deriving (Generic, Show)

-- }}}

-- Instances
-- {{{ Pretty instances
instance PP.Pretty Name where
  pretty (Unresolved i t) = "[" <> PP.pretty i <> "][?]." <> PP.pretty t
  pretty (Resolved i s t) =
    fold
      [ "["
      , PP.pretty i
      , "][:"
      , PP.pretty s
      , "]."
      , PP.pretty t
      ]

instance PP.Pretty TypeDefinition where
  pretty (TForeign i ty) =
    PP.hsep
      [ "[" <> PP.pretty i <> "]foreign"
      , "::"
      , PP.pretty ty
      ]
  pretty (TAlias i ty) = PP.hsep ["[" <> PP.pretty i <> "]alias", PP.pretty ty]
  pretty (TParam i) = "[" <> PP.pretty i <> "]Param"

instance PP.Pretty Scope where
  pretty (Scope{..}) =
    Cst.prettyTree
      "Scope"
      $ catMaybes
        [ guard (not $ null inherits) $> Cst.prettyTree "inherits" do
            PP.pretty <$> toList inherits
        , guard (not $ null types) $> Cst.prettyTree "types" do
            (k, v) ← HashMap.toList types
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        , guard (not $ null exprs) $> Cst.prettyTree "exprs" do
            (k, v) ← HashMap.toList exprs
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        ]

instance PP.Pretty ExprDefinition where
  pretty (EParam i) = "[" <> PP.pretty i <> "]Param"
  pretty (EForeign i ty) =
    PP.hsep
      [ "[" <> PP.pretty i <> "]Foreign"
      , "::"
      , PP.pretty ty
      ]
  pretty (EDeclaration i ty e) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Declaration")
      [ PP.pretty ty
      , PP.pretty e
      ]

instance PP.Pretty Type' where
  pretty (TyForall i scope name ty) =
    fold
      [ "["
      , PP.pretty i
      , "]∀"
      , PP.pretty name
      , ",[:"
      , PP.pretty scope
      , "] "
      , PP.pretty ty
      ]
  pretty (TyLambda i scope name ty) =
    fold
      [ "["
      , PP.pretty i
      , "]λ"
      , PP.pretty name
      , ",[:"
      , PP.pretty scope
      , "] "
      , PP.pretty ty
      ]
  pretty (TyArrow i kind from to) =
    fold
      [ "["
      , PP.pretty i
      , "]("
      , PP.pretty from
      , PP.pretty kind
      , PP.pretty to
      , ")"
      ]
  pretty (TyApp i f a) =
    fold
      [ "["
      , PP.pretty i
      , "]("
      , PP.pretty f
      , " "
      , PP.pretty a
      , ")"
      ]
  pretty (TyUnknown i) =
    fold
      [ "["
      , PP.pretty i
      , "]???"
      ]
  pretty (TyVar i name) =
    fold
      [ "["
      , PP.pretty i
      , "]"
      , PP.pretty name
      ]

instance PP.Pretty Expr where
  pretty (EUnknown i) = "[" <> PP.pretty i <> "]???"
  pretty (EHole i) =
    fold
      [ "["
      , PP.pretty i
      , "]_"
      ]
  pretty (EVar i name) = "[" <> PP.pretty i <> "]" <> PP.pretty name
  pretty (EMatch i exprs branches) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Match")
      $ fold
        [ guard (not $ null exprs) $> Cst.prettyTree "exprs" do
            PP.pretty <$> toList exprs
        , guard (not $ null branches) *> do
            (patterns, scope, expr) ← toList branches
            pure . Cst.prettyTree ("Branch[:" <> PP.pretty scope <> "]") . catMaybes $
              [ guard (not $ null patterns) $> Cst.prettyTree "patterns" do
                  PP.pretty <$> toList patterns
              , pure $ PP.pretty expr
              ]
        ]
  pretty (EApp i f a) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]App")
      [ PP.pretty f
      , PP.pretty a
      ]
  pretty _ = error "Unimplemented"

instance PP.Pretty Pattern where
  pretty (PWildcard i) = "[" <> PP.pretty i <> "]_"
  pretty (PName i n) = "[" <> PP.pretty i <> "]" <> PP.pretty n
  pretty (PProj i n args)
    | null args = "[" <> PP.pretty i <> "]." <> PP.pretty n
    | otherwise =
        fold
          [ "(["
          , PP.pretty i
          , "]."
          , PP.pretty n
          , " "
          , fold $ intersperse " " $ PP.pretty <$> toList args
          , ")"
          ]
instance (PP.Pretty a) ⇒ PP.Pretty (Definition a) where
  pretty (Definition i inner) =
    fold
      [ "["
      , PP.pretty i
      , "]"
      , PP.pretty inner
      ]

-- }}}
-- {{{ IsNode instances
instance IsNode TypeDefinition where
  nodeId (TForeign i _) = i
  nodeId (TAlias i _) = i
  nodeId (TParam i) = i

instance IsNode ExprDefinition where
  nodeId (EForeign i _) = i
  nodeId (EDeclaration i _ _) = i
  nodeId (EParam i) = i

instance IsNode (Definition a) where
  nodeId (Definition i _) = head i

instance IsNode Type' where
  nodeId (TyForall i _ _ _) = i
  nodeId (TyArrow i _ _ _) = i
  nodeId (TyApp i _ _) = i
  nodeId (TyVar i _) = i
  nodeId (TyLambda i _ _ _) = i
  nodeId (TyUnknown i) = i

instance IsNode Expr where
  nodeId (EVar i _) = i
  nodeId (EApp i _ _) = i
  nodeId (EMatch i _ _) = i
  nodeId (EUnknown i) = i
  nodeId (EPi i _ _ _ _ _) = i
  nodeId (EAnnotation i _ _) = i
  nodeId (EHole i) = i

instance IsNode Pattern where
  nodeId (PName i _) = i
  nodeId (PProj i _ _) = i
  nodeId (PWildcard i) = i

-- }}}
