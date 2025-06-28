module Nihil.Parser () where

import Text.Megaparsec qualified as M

data ParserState
  = ParserState {}

data ParserContext
  = ParserContext
  { minIndentation ∷ Int
  }

type Parser = ReaderT ParserContext (M.ParsecT Text Text (State ParserState))

data Comment = Comment
  { content ∷ Text
  , from ∷ M.SourcePos
  , to ∷ M.SourcePos
  }

data Token a
  = Token
  { comments ∷ [Comment]
  , value ∷ a
  }

parser ∷ ∀ a. Parser a → Parser a
parser = \a → a
