module Nihil.Cst.Base
  ( Span
  , Trivia (..)
  , Token (..)
  , Token'
  , Delimited (..)
  , Separated (..)
  , Name
  , HasSpan (..)
  , mkMegaparsecSpan
  , mkMegaparsecSpan'
  , prettyTree
  ) where

import Relude

import Error.Diagnose qualified as DG
import Nihil.Error qualified as Error
import Optics qualified as O
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M

type Span = Error.Span

-- | Converts from Megaparsec's positions, to Diagnose's span representation.
mkMegaparsecSpan ∷ M.SourcePos → M.SourcePos → Span
mkMegaparsecSpan from to =
  DG.Position
    { begin =
        ( M.unPos $ M.sourceLine from
        , M.unPos $ M.sourceColumn from
        )
    , end =
        ( M.unPos $ M.sourceLine to
        , let p = M.unPos (M.sourceColumn to)
           in if from == to then p + 1 else p
        )
    , file = M.sourceName from
    }

mkMegaparsecSpan' ∷ M.SourcePos → Span
mkMegaparsecSpan' from = mkMegaparsecSpan from from

-- | Auxilliary data attached to a token
data Trivia
  = TComment Span Text
  | TJunk Span Text
  deriving (Generic, Show)

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
  deriving (Generic, Show, Functor)

data Separated sep a = Separated
  { start ∷ M.SourcePos -- Required for the @HasSpan@ instance (the seq could be empty)
  , elements ∷ Seq (Either sep a)
  }
  deriving (Generic, Show)

---------- The @HasSpan@ typeclass
class HasSpan a where
  spanOf ∷ a → Span

instance HasSpan Span where
  spanOf = id

instance HasSpan (Token a) where
  spanOf = O.view #span

instance (HasSpan a, HasSpan b) ⇒ HasSpan (Either a b) where
  spanOf = either spanOf spanOf

instance (HasSpan a, HasSpan b) ⇒ HasSpan (a, b) where
  spanOf (a, b) = Error.mergeSpans (spanOf a) (spanOf b)

instance (HasSpan sep, HasSpan a) ⇒ HasSpan (Separated sep a) where
  spanOf (Separated start elements) =
    foldl'
      (\s e → Error.mergeSpans (spanOf e) s)
      (mkMegaparsecSpan' start)
      elements

---------- Pretty printing
prettyTree ∷ ∀ a. PP.Doc a → [PP.Doc a] → PP.Doc a
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
