module Nihil.Parser.Combinators
  ( pair
  , delimited
  , separated
  , delimitedList
  , manyTill
  , many'
  , string
  , name
  ) where

import Relude

import Data.Char qualified as Char
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Notation qualified as Notation
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

-- | The base building block for error-tolerant parsing. Attempts to parse two
-- things in sequence, eating away junk input coming our way.
--
-- Due to the way this is implemented, simply wrapping the second parser in
-- 'Text.Megaparsec.optional' will not work as expected. The second parser
-- could succeed right away, thus the first parser would assume to be missing
-- from the input.
pair
  ∷ ∀ a b
   . (Base.HasTrivia a, Base.HasTrivia b)
  ⇒ Core.LabelledParser a
  -- ^ The first step to parse
  → Core.LabelledParser b
  -- ^ The second step to parse
  → Core.Parser (Maybe a, Maybe b)
pair a b = Notation.tighten Notation.do
  mbA ← Notation.step a
  mbB ← Notation.step b
  Notation.return (mbA, mbB)

-- | Parse an expression of the form '(' inner ')', except the delimiters can
-- be anything (not just parenthesis).
delimited
  ∷ ∀ a
   . (Base.HasSpan a, Base.HasTrivia a)
  ⇒ Core.LabelledParser Base.Token'
  → Core.LabelledParser Base.Token'
  → Core.LabelledParser a
  → Core.Parser (Base.Delimited (Maybe a))
delimited open close inner = do
  o ← snd open
  offset ← M.getOffset
  (resultMb, c) ← Notation.tighten Notation.do
    result ← Notation.optStep inner
    c ← Notation.optStep close
    Notation.pure (result, c)
  offset' ← M.getOffset
  when (isNothing c) do
    Core.reportError
      "NoClosingDelimiter"
      (PP.hsep ["Missing closing delimiter."])
      ( catMaybes
          [ do
              result ← resultMb
              guard $ offset' - offset > 5
              pure
                ( Base.spanOf result
                , DG.Where $
                    PP.hsep
                      [ "This is how far I looked for the"
                      , PP.pretty $ fst close
                      , "."
                      ]
                )
          , Just
              ( Base.spanOf o
              , DG.This $
                  PP.hsep ["This", PP.pretty $ fst open, "is never closed."]
              )
          ]
      )
      []

  pure $ Base.Delimited o resultMb c

-- | Parse a list of "things" separated by some other separator. As an example,
-- one could parse a list of names via
--
-- @
-- separated False False (string ",") name
-- @
--
-- Not that this parse will keep consuming input until it reaches the context's
-- an element that satifies the context's 'stopOn' element
separated
  ∷ ∀ sep a
   . ( Base.HasSpan sep
     , Base.HasSpan a
     , Base.HasTrivia sep
     , Base.HasTrivia a
     )
  ⇒ Bool
  -- ^ Whether to allow a trailing separator
  → Core.LabelledParser sep
  → Core.LabelledParser a
  → Core.Parser (Base.Separated sep a)
separated allowTrailing sep inner = do
  start ← M.getSourcePos
  result ← go mempty
  startErrorSearch result
  pure $ Base.Separated start result
 where
  go acc = do
    mbResult ← Core.alsoStopOnPost sep $ Core.tryJunkTill inner
    mbSep ← Core.alsoStopOnPost inner $ Core.tryJunkTill sep

    if isJust mbSep || isJust mbResult
      then
        go
          . appendMb (Left <$> mbSep)
          . appendMb (Right <$> mbResult)
          $ acc
      else pure acc

  appendMb ∷ ∀ x. Maybe x → Seq x → Seq x
  appendMb a s = maybe s (s |>) a

  -- Similar to @errs@, but assumes it is being called at the very beginning.
  startErrorSearch ∷ Seq (Either sep a) → Core.Parser ()
  startErrorSearch (Left s :<| next) = do
    Core.reportError
      "LeadingSeparator"
      ( PP.hsep
          [ "Leading"
          , PP.pretty $ fst sep --  TODO: plural
          , "are not allowed."
          ]
      )
      [ (Base.spanOf s,) . DG.This $
          PP.hsep
            [ "I was expecting to find a"
            , PP.pretty $ fst inner
            , "before this"
            , PP.pretty $ fst sep
            , "."
            ]
      ]
      []
    errs next
  startErrorSearch res = errs res

  -- Looks for inconsistencies along the parsed list of separators / elements.
  errs ∷ Seq (Either sep a) → Core.Parser ()
  errs (Right e1 :<| next@(Right e2 :<| _)) = do
    Core.reportError
      "MissingSeparator"
      ( PP.hsep
          [ "There's a missing"
          , PP.pretty $ fst sep
          , "between two consecutive" -- TODO: plural
          , PP.pretty $ fst inner <> "."
          ]
      )
      [ (Base.spanOf e2,) . DG.Where $
          PP.hsep ["Second", PP.pretty $ fst inner]
      , (Base.spanOf e1,) . DG.Where $
          PP.hsep ["First", PP.pretty $ fst inner]
      ]
      []
    errs next
  errs (Left s1 :<| next@(Left s2 :<| _)) = do
    Core.reportError
      "MissingElement"
      ( PP.hsep
          [ "There's a missing"
          , PP.pretty $ fst inner
          , "between two consecutive" -- TODO: plural
          , PP.pretty $ fst sep <> "."
          ]
      )
      [ (Base.spanOf s2,) . DG.Where $
          PP.hsep ["Second", PP.pretty $ fst sep]
      , (Base.spanOf s1,) . DG.Where $
          PP.hsep ["First", PP.pretty $ fst sep]
      ]
      []
    errs next
  errs ((Left s) :<| Seq.Empty)
    | not allowTrailing =
        Core.reportError
          "TraillingSeparator"
          ( PP.hsep
              [ "Trailing"
              , PP.pretty $ fst sep -- TODO: plural
              , "are not allowed in this context."
              ]
          )
          [(Base.spanOf s, DG.This $ PP.hsep ["The", PP.pretty $ fst sep])]
          []
  errs (_ :<| rest) = errs rest
  errs _ = pure ()

-- | Wrapper around @'delimited' and @'separated'.
delimitedList
  ∷ ∀ a sep
   . ( Base.HasSpan a
     , Base.HasSpan sep
     , Base.HasTrivia sep
     , Base.HasTrivia a
     )
  ⇒ Core.LabelledParser Base.Token'
  → Core.LabelledParser Base.Token'
  → Core.LabelledParser sep
  → Core.LabelledParser a
  → Core.Parser (Base.Delimited (Base.Separated sep a))
delimitedList open close sep inner = do
  f ← M.getSourcePos
  res ←
    delimited open close $
      Core.label (fst inner <> " list") $
        separated True sep inner

  -- Not the best estimate, but oh well...
  let defaultStart = Base.Separated f mempty
  pure $ fmap (fromMaybe $ defaultStart) res

-- | Keep running a parser until it returns a "Maybe"
many' ∷ ∀ a. Core.Parser (Maybe a) → Core.Parser (Seq a)
many' inner = go mempty
 where
  go acc = do
    mbInner ← inner
    case mbInner of
      Just i → go (acc |> i)
      Nothing → pure acc

manyTill
  ∷ ∀ a stop
   . (Base.HasTrivia stop, Base.HasTrivia a)
  ⇒ Core.LabelledParser a
  → Core.LabelledParser stop
  → Core.Parser (Seq a, Maybe stop)
manyTill inner stop = go mempty
 where
  go acc = do
    result ←
      Core.tryJunkTill
        . (fst inner,)
        . M.choice
        $ [ Left <$> snd stop
          , Right <$> snd inner
          ]
    case result of
      Just (Right i) → go (acc |> i)
      Just (Left s) → pure (acc, Just s)
      Nothing → pure (acc, Nothing)

string ∷ Text → Core.LabelledParser Base.Token'
string s = Core.label s . Core.token . MC.string $ s

name ∷ Core.Parser Base.Name
name = Core.token $ M.try do
  -- TODO: support more chars
  let chunk = M.takeWhile1P (Just "character") \c → Char.isAlphaNum c
  let sep = MC.string "."
  offset ← M.getOffset
  -- TODO: restructure this to have actual error messages (for instance, for trailing dots)
  (s, result) ← Core.spanned $ Text.intercalate "." <$> M.sepBy1 chunk sep
  when (elem result illegal) $ do
    Core.throwError
      offset
      "InvalidName"
      ( PP.hsep
          [ PP.dquotes $ PP.pretty result
          , "is a keyword, hence not a valid name."
          ]
      )
      [(s, DG.This "This is where I found the name.")]
      []
  pure result
 where
  illegal =
    [ "module"
    , "import"
    , "foreign"
    , "type"
    , "struct"
    , "inductive"
    , "trait"
    , "where"
    , "text"
    , "do"
    , "with"
    , "if"
    , "make"
    ]
