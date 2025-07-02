module Nihil.Parser.Combinators
  ( pair
  , delimited
  , separated
  , delimitedList
  , manyTill
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
   . Core.LabelledParser a
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
   . (Base.HasSpan a)
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
      offset
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
   . (Base.HasSpan sep, Base.HasSpan a)
  ⇒ Bool
  -- ^ Whether to allow a trailing separator
  → Core.LabelledParser sep
  → Core.LabelledParser a
  → Core.Parser (Base.Separated sep a)
separated allowTrailing sep inner = do
  start ← M.getSourcePos
  result ← go mempty
  startErrorSearch result
  pure $ Base.Separated start $ snd <$> result
 where
  go acc = do
    offset ← M.getOffset
    mbResult ← Core.alsoStopOnPost (snd sep) $ Core.tryJunkTill False inner

    offset' ← M.getOffset
    mbSep ← Core.alsoStopOnPost (snd inner) $ Core.tryJunkTill False sep

    if isJust mbSep || isJust mbResult
      then
        go
          . appendMb ((offset,) . Left <$> mbSep)
          . appendMb ((offset',) . Right <$> mbResult)
          $ acc
      else pure acc

  appendMb ∷ ∀ x. Maybe x → Seq x → Seq x
  appendMb a s = maybe s (s |>) a

  -- Similar to @errs@, but assumes it is being called at the very beginning.
  startErrorSearch ∷ Seq (Int, Either sep a) → Core.Parser ()
  startErrorSearch ((o, Left s) :<| next) = do
    Core.reportError
      o
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
  errs ∷ Seq (Int, Either sep a) → Core.Parser ()
  errs ((_, Right e1) :<| next@((o2, Right e2) :<| _)) = do
    Core.reportError
      o2
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
  errs ((_, Left s1) :<| next@((o2, Left s2) :<| _)) = do
    Core.reportError
      o2
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
  errs ((o, Left s) :<| Seq.Empty)
    | not allowTrailing =
        Core.reportError
          o
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
   . (Base.HasSpan a, Base.HasSpan sep)
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

manyTill
  ∷ ∀ a stop
   . Core.LabelledParser a
  → Core.LabelledParser stop
  → Core.Parser (Seq a, Maybe stop)
manyTill inner stop = go mempty
 where
  go acc = do
    mbStop ← M.optional $ snd stop
    case mbStop of
      Just s → pure (acc, Just s)
      Nothing → do
        mbInner ← Core.alsoStopOnPre (snd stop) $ Core.tryJunkTill False inner
        -- _ ← Core.alsoStopOnPre (snd stop) $ M.optional $ M.lookAhead $ snd inner
        -- mbInner ← Core.alsoStopOnPre (snd stop) $ M.optional $ snd inner
        case mbInner of
          Just i → go (acc |> i)
          Nothing → do
            offset ← M.getOffset
            pos ← M.getSourcePos
            Core.reportError
              offset
              "NoEnd"
              ( PP.hsep
                  [ "I was expecting to find a"
                  , PP.pretty $ fst stop
                  , "after all these"
                  , PP.pretty $ fst inner
                  , "."
                  ]
              )
              [
                ( Base.mkMegaparsecSpan' pos
                , DG.This "I managed to get this far looking for one."
                )
              ]
              []
            pure (acc, Nothing)

string ∷ Text → Core.LabelledParser Base.Token'
string s = Core.label s . Core.token . MC.string $ s

name ∷ Core.Parser Base.Name
name = Core.token $ M.try do
  -- TODO: support more chars
  let chunk = M.takeWhile1P (Just "character") \c → Char.isAlphaNum c
  let sep = MC.string "."
  offset ← M.getOffset
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
