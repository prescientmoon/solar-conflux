-- | This module implements qualified applicative-do notation for
-- error-tolerant parsing.
--
-- Example usage:
-- @
-- Notation.tighten Notation.do
--   a <- Core.name
--   b <- Core.string "and"
--   Notation.pure (a, b)
-- @
module Nihil.Parser.Notation
  ( tighten
  , fmap
  , pure
  , return
  , step
  , optStep
  , pre
  , (<*>)
  , (>>=)
  ) where

import Relude hiding (fmap, pure, return, (<*>), (>>=))
import Relude qualified as Relude

import Nihil.Parser.Core (LabelledParser)
import Nihil.Parser.Core qualified as Core
import Type.Errors (DelayError, ErrorMessage (..))

data LooseParser a
  = LooseParser
      -- Parsers coming from the future, of things we are going to parse next
      -- and are not allowed to parse now.
      --
      -- The former one runs before the actual parser; the latter runs after.
      ( (Core.Parser (), Core.Parser ())
        -- Whether the previous step was successful.
        → Bool
        -- Resulting parser, annotated with whether it's been successful.
        → Core.Parser (a, Bool)
      )

return ∷ ∀ a. a → LooseParser a
return a = LooseParser \_ _ → Relude.pure (a, True)

pure ∷ ∀ a. a → LooseParser a
pure = return

infixl 4 <*>

-- | The primary function qualified-applicative-do will get desugared to.
(<*>)
  ∷ ∀ a b
   . LooseParser (Maybe a → b)
  → LooseParsingStep a
  → LooseParser b
(<*>) (LooseParser f) (LooseParsingStep s opt timing) =
  LooseParser \p sr → do
    let p' = case timing of
          Core.Pre → first (<|> void (snd s)) p
          Core.Post → second (<|> void (snd s)) p
    (f', sr') ← f p' sr
    -- a' ← Core.alsoStopOn p $ Core.tryJunkTill (not opt) s
    a' ← Core.alsoStopOn p $ Core.tryJunkTill (not opt && sr') s
    Relude.pure (f' a', isJust a' || (opt && sr'))

fmap
  ∷ ∀ a b
   . (Maybe a → b)
  → LooseParsingStep a
  → LooseParser b
fmap f (LooseParsingStep s opt _) = LooseParser \p sr → do
  a' ← Core.alsoStopOn p $ Core.tryJunkTill (not opt && sr) s
  Relude.pure (f a', isJust a' || (opt && sr))

tighten ∷ ∀ a. LooseParser a → Core.Parser a
tighten (LooseParser p) = fst <$> p (empty, empty) True

-- | We have to implement one of those, as qualified-do will refuse to work
-- otherwise (even if we don't write any code that desugars to it).
(>>=) ∷ (DelayError ('Text "LooseParser is not a monad!")) ⇒ Void → Void → Void
(>>=) a _ = a

-- | Individual step in a loose parsing pipeline.
data LooseParsingStep a = LooseParsingStep
  { parser ∷ LabelledParser a
  , optional ∷ Bool
  , timing ∷ Core.StopOnTiming
  }
  deriving (Generic)

step ∷ ∀ a. LabelledParser a → LooseParsingStep a
step p = LooseParsingStep p False Core.Post

optStep ∷ ∀ a. LabelledParser a → LooseParsingStep a
optStep p = LooseParsingStep p True Core.Post

-- | Makes the parser attempt to run before the ones that came before it.
--
-- Mostly useful for things like keywords, which might otherwise trigger
-- error messages coming from names.
pre ∷ ∀ a. LooseParsingStep a → LooseParsingStep a
pre (LooseParsingStep p sr _) = LooseParsingStep p sr Core.Pre
