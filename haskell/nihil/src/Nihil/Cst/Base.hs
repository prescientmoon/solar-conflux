module Nihil.Cst.Base
  ( Span
  , Trivia (..)
  , Token (..)
  , Token'
  , Delimited (..)
  , Separated (..)
  , Name
  , HasTrivia
  , attachTrivia
  , spanOf
  , allTrivia
  , mkMegaparsecSpan
  , mkMegaparsecSpan'
  , prettyTree
  ) where

import Relude

import GHC.Generics ((:*:) ((:*:)), (:+:))
import GHC.Generics qualified as Generic
import GHC.Generics.Optics qualified as O
import Nihil.Error qualified as Error
import Nihil.Utils (adjoinMany)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M

type Span = Error.Span

mkMegaparsecSpan ∷ M.SourcePos → M.SourcePos → Span
mkMegaparsecSpan = coerce Error.mkSpan

mkMegaparsecSpan' ∷ M.SourcePos → Span
mkMegaparsecSpan' = coerce Error.mkSpan'

-- | Auxilliary data attached to a token
data Trivia
  = TComment Span Text
  | TJunk Span Text
  deriving (Generic, Show, HasTrivia)

data Token a
  = Token
  { trivia ∷ Seq Trivia
  , span ∷ Span
  , value ∷ a
  }
  deriving (Generic, Show, Functor)

type Token' = Token Text
type Name = Token Text

data Delimited a = Delimited
  { open ∷ Token'
  , inner ∷ a
  , close ∷ Maybe Token'
  }
  deriving (Generic, Show, Functor, Foldable, Traversable, HasTrivia)

data Separated sep a = Separated
  { start ∷ M.SourcePos
  , elements ∷ Seq (Either sep a)
  }
  deriving (Generic, Show, Functor, Foldable, Traversable, HasTrivia)

---------- Trivia attachments
class HasTrivia a where
  mbSpanOf ∷ a → Maybe Span
  default mbSpanOf ∷ (Generic a, GHasTrivia (Generic.Rep a)) ⇒ a → Maybe Span
  mbSpanOf = gMbSpanOf . Generic.from

  allTrivia ∷ O.Traversal' a (Seq Trivia)
  default allTrivia ∷ (Generic a, GHasTrivia (Generic.Rep a)) ⇒ O.Traversal' a (Seq Trivia)
  allTrivia = O.generic % gAllTrivia

spanOf ∷ ∀ a. (HasTrivia a) ⇒ a → Span
spanOf x = case mbSpanOf x of
  Just s → s
  Nothing → error "Used `spanOf` on an empty CST node"

attachTrivia ∷ ∀ a. (HasTrivia a) ⇒ Seq Trivia → a → Maybe a
attachTrivia t a = case O.preview optic a of
  Just _ → Just $ O.over optic (t <>) a
  Nothing → Nothing
 where
  optic = O.singular $ allTrivia @a

asTraversal
  ∷ ∀ k s t a b
   . (O.Is k O.A_Traversal)
  ⇒ O.Optic k O.NoIx s t a b
  → O.Optic O.A_Traversal O.NoIx s t a b
asTraversal = O.castOptic @O.A_Traversal

instance HasTrivia Span where
  mbSpanOf = Just
  allTrivia = O.castOptic @O.A_Traversal $ O.noIx O.ignored

instance HasTrivia M.SourcePos where
  mbSpanOf = Just . mkMegaparsecSpan'
  allTrivia = asTraversal $ O.noIx O.ignored

instance HasTrivia Text where
  mbSpanOf _ = Nothing
  allTrivia = asTraversal $ O.noIx O.ignored

instance HasTrivia (Token a) where
  mbSpanOf = Just . O.view #span
  allTrivia = asTraversal #trivia

instance (HasTrivia a) ⇒ HasTrivia (Seq a) where
  mbSpanOf = fold . fmap mbSpanOf
  allTrivia = O.traversed % allTrivia

deriving instance (HasTrivia a) ⇒ HasTrivia (Maybe a)
deriving instance (HasTrivia a) ⇒ HasTrivia [a]
deriving instance (HasTrivia l, HasTrivia r) ⇒ HasTrivia (Either l r)
deriving instance (HasTrivia l, HasTrivia r) ⇒ HasTrivia (l, r)
deriving instance (HasTrivia a, HasTrivia b, HasTrivia c) ⇒ HasTrivia (a, b, c)

-- Generic deriving
class GHasTrivia f where
  gMbSpanOf ∷ ∀ a. f a → Maybe Span
  gAllTrivia ∷ ∀ a. O.Traversal' (f a) (Seq Trivia)

instance (GHasTrivia f) ⇒ GHasTrivia (Generic.M1 i c f) where
  gMbSpanOf (Generic.M1 i) = gMbSpanOf i

  gAllTrivia ∷ ∀ a. O.Traversal' (Generic.M1 i c f a) (Seq Trivia)
  gAllTrivia = O.coerced % gAllTrivia @f @a

instance (HasTrivia c) ⇒ GHasTrivia (Generic.K1 i c) where
  gMbSpanOf (Generic.K1 i) = mbSpanOf i
  gAllTrivia = O.coerced % allTrivia @c

instance GHasTrivia Generic.U1 where
  gMbSpanOf _ = Nothing
  gAllTrivia = asTraversal $ O.noIx O.ignored

instance (GHasTrivia f, GHasTrivia g) ⇒ GHasTrivia (f :+: g) where
  gMbSpanOf (Generic.L1 l) = gMbSpanOf l
  gMbSpanOf (Generic.R1 r) = gMbSpanOf r

  gAllTrivia =
    adjoinMany
      [ O._L1 % gAllTrivia
      , O._R1 % gAllTrivia
      ]

instance (GHasTrivia f, GHasTrivia g) ⇒ GHasTrivia (f :*: g) where
  gMbSpanOf (l :*: r) = gMbSpanOf l <> gMbSpanOf r
  gAllTrivia =
    adjoinMany
      [ O.gposition @1 % gAllTrivia
      , O.gposition @2 % gAllTrivia
      ]

---------- Pretty printing
prettyTree ∷ ∀ a. PP.Doc a → [PP.Doc a] → PP.Doc a
prettyTree label [] = label
prettyTree label members =
  PP.vsep
    [ label <> ":"
    , PP.indent 2 $ PP.vsep $ members
    ]

instance (PP.Pretty a) ⇒ PP.Pretty (Token a) where
  pretty t =
    PP.vsep $
      trivia
        <> [ PP.hsep
              [ PP.dquotes $ PP.pretty (O.view #value t)
              , PP.parens (PP.pretty $ O.view #span t)
              ]
           ]
   where
    trivia =
      toList $
        (\e → "vvv " <> PP.pretty e)
          <$> O.view #trivia t

instance PP.Pretty Trivia where
  pretty (TComment s t) =
    PP.hsep
      [ "Comment:"
      , PP.dquotes $ PP.pretty t
      , PP.parens (PP.pretty s)
      ]
  pretty (TJunk s t) =
    PP.hsep
      [ "Junk:"
      , PP.dquotes $ PP.pretty t
      , PP.parens (PP.pretty s)
      ]

instance (PP.Pretty a) ⇒ PP.Pretty (Delimited a) where
  pretty (Delimited{..}) =
    prettyTree
      "Delimited"
      [ PP.pretty open
      , PP.pretty inner
      , maybeToMonoid $ PP.pretty <$> close
      ]

instance (PP.Pretty sep, PP.Pretty a) ⇒ PP.Pretty (Separated sep a) where
  pretty (Separated _ elements) =
    prettyTree
      "Separated"
      $ toList
      $ either PP.pretty PP.pretty <$> elements
