module Nihil.Cst.Base
  ( Span (..)
  , Trivia (..)
  , Token (..)
  , Delimited (..)
  , Separated (..)
  , Name
  ) where

import Text.Megaparsec qualified as M

data Span = Span
  { from ∷ M.SourcePos
  , to ∷ M.SourcePos
  }
  deriving (Generic, Show)

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
  deriving (Generic, Show)

type Name = Token Text

data Delimited a = Delimited
  { tOpen ∷ Token ()
  , inner ∷ a
  , tClose ∷ Token ()
  }
  deriving (Generic, Show)

data Separated sep a = Separated
  { first ∷ Maybe a
  , rest ∷ [(sep, Maybe a)]
  }
  deriving (Generic, Show)
