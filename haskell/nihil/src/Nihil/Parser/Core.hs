module Nihil.Parser.Core
  ( LabelledParser
  , ParserState (..)
  , ParserContext (..)
  , Parser
  , StopOnTiming (..)
  , reportError
  , throwError
  , reportExpectedError
  , spanned
  , token
  , parseTest
  , alsoStopOn
  , alsoStopOnPre
  , alsoStopOnPost
  , resetStopOn
  , withIndentation
  , exactlyIndented
  , blockLike
  , mkBlock
  , badIndent
  , saveTrivia
  , checkIndentation
  , sc
  , label
  , junkTill
  , tryJunkTill
  ) where

import Relude

import Data.Char qualified as Char
import Data.Foldable1 (foldl1)
import Data.Sequence ((|>))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Error qualified as Error
import Nihil.Utils (textPretty)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Internal qualified as MI
import Text.Megaparsec.State qualified as M

---------- The parser monad

data ParserState
  = ParserState
  { trivia ∷ Seq Base.Trivia
  -- ^ trivia that have been parsed, but are yet to get attached to some token.
  , exactIndentation ∷ Bool
  -- ^ When 'False', tokens must be indented strictly more than 'indentation'.
  -- When `True`, the first token of the incoming stream must be indented
  -- precisely equal to `indentation`.
  }
  deriving (Generic, Show)

data ParserContext
  = ParserContext
  { indentation ∷ M.Pos
  -- ^ All tokens must start at this column (or to the right)
  , stopOn ∷ (Parser (), Parser ())
  -- ^ When consuming junk input, stop when this parser succeeds.
  --
  -- The first element of the pair runs before the parser we're currently
  -- actively working on, and the one on the right runs after.
  }
  deriving (Generic)

-- | Tells functions like @'alsoStopOn' whether to run the parser before the
-- thing we're currently parsing, or after.
data StopOnTiming = Pre | Post

-- | Wrapper around @'Error.Report', as it doesn't have an @'Ord' instance.
data Error = Error Int Error.Report

-- NOTE: this instance is not correct, but I added it nonetheless in order to
-- satisfy the compiler 😭 (why doesn't @'Error.Report' have one???).
instance Eq Error where
  (Error a _) == (Error b _) = a == b

instance Ord Error where
  compare (Error a _) (Error b _) = compare a b

type Parser =
  ReaderT
    ParserContext
    (StateT ParserState (M.Parsec Error Text))

-- | Save a piece of trivia to the parser state. The piece will later on get
-- associated with the next token, and kept inside the CST.
saveTrivia ∷ Base.Trivia → Parser ()
saveTrivia c = O.modifying #trivia (|> c)

-- | Update the minimum indentation of some parser.
withIndentation ∷ ∀ a. M.Pos → Parser a → Parser a
withIndentation level = local (O.set #indentation level)

-- | Tell a parser to stop consuming junk when hitting a certain element.
alsoStopOn ∷ ∀ a. (Parser (), Parser ()) → Parser a → Parser a
alsoStopOn (pre, post) =
  local $
    O.over (#stopOn % O._1) (<|> M.lookAhead pre)
      . O.over (#stopOn % O._2) (<|> M.lookAhead post)

-- | Tell a parser to stop consuming junk when hitting a certain element.
--
-- The given parser runs before the one we're actively trying to complete.
alsoStopOnPre ∷ ∀ stop a. Parser stop → Parser a → Parser a
alsoStopOnPre pre = local $ O.over (#stopOn % O._1) (<|> void (M.lookAhead pre))

-- | Tell a parser to stop consuming junk when hitting a certain element.
--
-- The given parser runs before the one we're actively trying to complete.
alsoStopOnPost ∷ ∀ stop a. Parser stop → Parser a → Parser a
alsoStopOnPost post = local $ O.over (#stopOn % O._2) (<|> void (M.lookAhead post))

-- | Tells a parser to only stop on bad indentation & EOF.
resetStopOn ∷ ∀ a. Parser a → Parser a
resetStopOn = local (O.set #stopOn (empty, badIndent <|> M.eof))

-- | Create a custom error. The offset is there for sorting purposes.
reportError
  ∷ Int
  → Text
  → Error.Doc
  → [(Error.Span, DG.Marker Error.Doc)]
  → [DG.Note Error.Doc]
  → Parser ()
reportError offset code desc markers hints =
  M.region (M.setErrorOffset offset)
    . M.registerFancyFailure
    . Set.singleton
    . M.ErrorCustom
    $ Error offset
    $ DG.Err (Just . PP.pretty $ "Parser" <> code) desc markers hints

throwError
  ∷ Int
  → Text
  → Error.Doc
  → [(Error.Span, DG.Marker Error.Doc)]
  → [DG.Note Error.Doc]
  → Parser ()
throwError offset code desc markers hints =
  M.region (M.setErrorOffset offset)
    . M.registerFancyFailure
    . Set.singleton
    . M.ErrorCustom
    $ Error offset
    $ DG.Err (Just . PP.pretty $ "Parser" <> code) desc markers hints

-- | Attempt to run a parser on some string, reporting the results to stdout.
-- An error is reported if the parser does not consume the full string it's
-- given. The parser state at the very end of the parse is also shown.
parseTest ∷ ∀ a. (PP.Pretty a) ⇒ Parser a → Text → IO ()
parseTest p input = do
  let initialCtx = ParserContext (M.mkPos 1) (empty, empty)
  let initialState = ParserState mempty True

  let filename = "<test>"
  let readerResult = runReaderT (resetStopOn $ sc *> p <* M.eof) initialCtx
  let stateResult = runStateT readerResult initialState
  let (errs, result) = runParser stateResult filename input

  for_ result \(x, finalState) → do
    putTextLn $ textPretty finalState
    putTextLn $ textPretty x

  Error.printDiagnostic $
    DG.addFile
      (Error.addReports errs)
      filename
      (Text.unpack input)

runParser
  ∷ ∀ a
   . M.Parsec Error Text a
  → String
  → Text
  → ([Error.Report], Maybe a)
runParser p s i = runIdentity do
  (MI.Reply s' _ result) ← MI.runParsecT p (M.initialState s i)
  let bundle es = finalizeError =<< sortWith M.errorOffset es
  pure $ case result of
    MI.OK _ x → (bundle (M.stateParseErrors s'), Just x)
    MI.Error e → (bundle $ e : M.stateParseErrors s', Nothing)
 where
  finalizeError ∷ M.ParseError Text Error → [Error.Report]
  finalizeError (M.TrivialError offset Nothing expected) =
    pure $
      DG.Err
        (Just "ParserUnexpectedToken")
        ("Unexpected token.")
        [(mkPos offset, DG.This $ printExpected expected)]
        []
  finalizeError (M.TrivialError offset (Just thing) expected) =
    pure $
      DG.Err
        (Just "ParserUnexpectedToken")
        ("Unexpected " <> prettyItem thing <> ".")
        [(mkPos offset, DG.This $ printExpected expected)]
        []
  finalizeError (M.FancyError offset components) =
    Set.toList components <&> \case
      M.ErrorCustom (Error _ custom) → custom
      M.ErrorFail msg →
        DG.Err
          (Just "ParserMonadFail")
          "The fail function got invoked while parsing."
          [(mkPos offset, DG.This $ PP.pretty msg)]
          [shouldNeverSeeThis]
      M.ErrorIndentation ordering expected actual →
        DG.Err
          (Just "ParserIllegalIndentation")
          ( PP.hsep
              [ "Expected an indentation level of"
                  <> case ordering of
                    EQ → ""
                    GT → " more than"
                    LT → " less than"
              , PP.pretty (M.unPos expected) <> ". Got"
              , PP.pretty $ M.unPos actual
              , "instead."
              ]
          )
          [ (iSpan expected, DG.Where $ "Expected indentation")
          , (iSpan actual, DG.This $ "Actual indentation")
          ]
          [shouldNeverSeeThis]
   where
    -- The span for an indentation error
    iSpan ∷ M.Pos → Base.Span
    iSpan pos =
      O.set (#end % O._2) (M.unPos pos)
        . O.set (#begin % O._2) 1
        $ mkPos offset

  shouldNeverSeeThis ∷ DG.Note Error.Doc
  shouldNeverSeeThis = DG.Note "You should never see this in practice."

  mkPos ∷ Int → Base.Span
  mkPos offset = Base.mkMegaparsecSpan pos pos
   where
    pos =
      M.pstateSourcePos $
        M.reachOffsetNoLine offset $
          M.statePosState $
            M.initialState s i

  printExpected ∷ Set (M.ErrorItem Char) → Error.Doc
  printExpected variants
    | Set.size variants == 0 = "No idea what this is supposed to be..."
    | Set.size variants == 1 =
        PP.group $
          PP.sep
            [ "Expected"
            , maybeToMonoid
                . fmap prettyItem
                . viaNonEmpty head
                $ Set.toList variants
            , "."
            ]
    | otherwise =
        fold
          [ "Expected one of "
          , PP.sep
              . intersperse ","
              . fmap prettyItem
              $ Set.toList variants
          , "."
          ]

  prettyItem = PP.dquotes . PP.pretty . M.showErrorItem (Proxy @Text)

reportExpectedError ∷ Int → Error.Span → Text → Parser ()
reportExpectedError offset s l = do
  reportError
    offset
    ("Expected")
    (PP.hsep ["Unexpected token."])
    [
      ( s
      , DG.This $
          PP.hsep
            [ "I was supposed to find a"
            , PP.pretty l
            , "by now."
            ]
      )
    ]
    []

---------- Core combinators

-- | Turn a parser into one that keeps track of the source span it's consumed.
spanned ∷ ∀ a. Parser a → Parser (Base.Span, a)
spanned p = do
  start ← M.getSourcePos
  inner ← p
  end ← M.getSourcePos
  pure (Base.mkMegaparsecSpan start end, inner)

-- | Turns a parser into one which keeps track of the source spam it's
-- consumed, and of the trivia that came before. The resulting parser will
-- consume spaces  and trivia that come after it, similarly to
-- 'Text.Megaparsec.Char.Lexer.lexeme' (this is also more performant, as it
-- means we don't have to keep parsing the same trivia over and over again).
--
-- | The resulting parser will also check whether its indentation matches
-- the expected amount.
token ∷ ∀ a. Parser a → Parser (Base.Token a)
token p = do
  void checkIndentation
  (innerSpan, inner) ← spanned p
  O.assign #exactIndentation False

  -- Take the saved trivia, and reset the list
  trivia ← O.use #trivia
  O.assign #trivia mempty
  sc

  pure $
    Base.Token
      { value = inner
      , trivia = trivia
      , span = innerSpan
      }

-- | Ensures the current indentation is at least the minimum allowed level.
checkIndentation ∷ Parser M.Pos
checkIndentation = do
  col ← O.view #sourceColumn <$> M.getSourcePos
  indentation ← O.gview #indentation
  exact ← O.use #exactIndentation
  if exact
    then do
      when (col /= indentation) do
        M.fancyFailure . Set.singleton $
          M.ErrorIndentation EQ indentation col
    else when (col <= indentation) do
      M.fancyFailure . Set.singleton $
        M.ErrorIndentation GT indentation col
  pure col

-- | Parse space characters / trivia. Although it might look at it, not all
-- results are thrown away. Parsed trivia are saved in the parser state.
sc ∷ Parser ()
sc =
  M.skipMany $
    M.choice
      [ M.hidden M.space1
      , M.hidden comment
      ]

-- | Parse a comment, and save it to the parser's state. The comment will be
-- saved together with the next token inside the CST.
--
-- | Tokens at the end of the file will be saved together with the EOF token (see
-- 'Nihil.CST.Module.Module.eof').
comment ∷ Parser ()
comment = do
  (triviapan, content) ← spanned (lineComment <|> blockComment)
  saveTrivia $ Base.TComment triviapan content
  pure ()

-- | Parse a line comment, returning its content. Specialized version of
-- 'Text.Megaparsec.Char.Lexer.skipLineComment.
lineComment ∷ Parser Text
lineComment = do
  void $ M.string "--"
  M.takeWhileP (Just "character") (/= '\n')

-- | Parse a (possibly nested) block comment, returning its content.
-- Specialized version of 'Text.Megaparsec.Char.Lexer.skipBlockComment'.
blockComment ∷ Parser Text
blockComment = do
  void $ M.string "{-"
  content ← M.manyTill chunk end
  pure $ fold content
 where
  chunk = blockComment <|> fmap Text.singleton M.anySingle
  end = M.string "-}"

-- | Here to go around Haskell's impredicative type issues.
newtype BlockMaker = BlockMaker (∀ a. Parser a → Parser a)

-- | Creates a parsers for a series of things that are aligned to eachother
-- horizontally. Examples include:
-- - pattern match branches
-- - statements in some block
-- - declarations in a module
blockLike ∷ Parser BlockMaker
blockLike =
  checkIndentation <&> \expected → BlockMaker \p → do
    O.assign #exactIndentation True
    r ← withIndentation expected p
    O.assign #exactIndentation False
    pure r

mkBlock ∷ ∀ a. BlockMaker → Parser a → Parser a
mkBlock (BlockMaker f) a = f a

-- | Turns a parser into one with strict indentation enabled. Check
-- `ParserState.exactIndentation` for details regarding what this means.
exactlyIndented ∷ ∀ a. Parser a → Parser a
exactlyIndented p = blockLike >>= flip mkBlock p

-- | A parser which succeeds precisely when the indentation rules are violated.
badIndent ∷ Parser ()
badIndent = M.notFollowedBy (void checkIndentation)

---------- Labelled parsers

-- | Like @'Parser', but contains an additional label for errors.
type LabelledParser a = (Text, Parser a)

label ∷ ∀ a. Text → Parser a → LabelledParser a
label l p = (l, M.label (Text.unpack l) p)

---------- Error-tolerant parsing

-- | Consume text, annotating it as junk, until the given parser succeeds.
junkTill ∷ ∀ a. LabelledParser a → Parser a
junkTill (l, p) = do
  offset ← M.getOffset
  -- TODO: avoid the lookahead here for performance?
  chunks ← M.manyTill single (M.lookAhead p)

  if
    | Just neChunks ← nonEmpty chunks → do
        let chunksSpan = foldl1 Error.mergeSpans $ fst <$> neChunks
        saveTrivia $ Base.TJunk chunksSpan (foldMap snd chunks)
        reportError
          offset
          "Junk"
          ( PP.hsep
              [ "I was looking for a"
              , PP.dquotes $ PP.pretty l
              , "when I came across a bunch of unreadable characters."
              ]
          )
          [(chunksSpan, DG.This "I have no idea what this is supposed to mean.")]
          []
    | otherwise → pure ()

  p
 where
  single ∷ Parser (Error.Span, Text)
  single =
    spanned chunk
      <* sc
      <* O.assign #exactIndentation False

  chunk ∷ Parser Text
  chunk = do
    h ← M.anySingle
    if Char.isAlpha h
      then do
        more ← M.takeWhile1P (Just "junk") Char.isAlpha
        pure $ Text.singleton h <> more
      else pure $ Text.singleton h

-- | Similar to 'junkTill', except will automatically stop when encountering
-- an element matching the context's 'stopOn' parser.
tryJunkTill
  ∷ ∀ a
   . Bool
  -- ^ Whether to report an error on @`Nothing`.
  → LabelledParser a
  → Parser (Maybe a)
tryJunkTill shouldReport (l, inner) = do
  void checkIndentation

  offset ← M.getOffset
  (pre, post) ← O.gview #stopOn

  (resultSpan, result) ←
    spanned $
      junkTill . (l,) . M.choice $
        [ Nothing <$ pre
        , Just <$> inner
        , Nothing <$ post
        ]

  when (shouldReport && isNothing result) $
    reportExpectedError offset resultSpan l

  pure result

---------- Pretty printing
instance PP.Pretty ParserState where
  pretty st =
    Base.prettyTree
      "ParserState"
      [ PP.pretty ("exactIndentation" ∷ Text, O.view #exactIndentation st)
      , PP.pretty ("trivia" ∷ Text, toList $ O.view #trivia st)
      ]
