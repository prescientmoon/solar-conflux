module Nihil.Parser.Core
  ( LabelledParser
  , KeyedParser
  , ParserState (..)
  , ParserContext (..)
  , Parser
  , StopOnTiming (..)
  , BlockMaker
  , keyedAppend
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
  , anyIndentation
  , unindented
  , mkBlock
  , tryMkBlock
  , blockLike
  , badIndent
  , saveTrivia
  , checkIndentation
  , sc
  , label
  , junkTill
  , tryJunkTill
  , tryReportedJunkTill
  , noHints
  ) where

import Relude

import Data.Char qualified as Char
import Data.Foldable1 (foldl1)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
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
data IndentationRelation
  = IGT
  | IGTE
  | IEQ
  | IAny
  deriving (Generic, Show)

data ParserState
  = ParserState
  { trivia ∷ Seq Base.Trivia
  -- ^ trivia that have been parsed, but are yet to get attached to some token.
  , relation ∷ IndentationRelation
  }
  deriving (Generic, Show)

data ParserContext
  = ParserContext
  { indentation ∷ M.Pos
  -- ^ All tokens must start at this column (or to the right)
  , stopOn ∷ (KeyedParser (), KeyedParser ())
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

instance M.ShowErrorComponent Error where
  showErrorComponent (Error _ r) =
    Text.unpack
      . Error.showDiagnostic
      . Error.addReports
      . pure
      $ r

type Parser =
  ReaderT
    ParserContext
    (StateT ParserState (M.Parsec Error Text))

-- | Save a piece of trivia to the parser state. The piece will later on get
-- associated with the next token, and kept inside the CST.
saveTrivia ∷ Base.Trivia → Parser ()
saveTrivia c = O.modifying #trivia (|> c)
{-# INLINE saveTrivia #-}

-- | Update the minimum indentation of some parser.
withIndentation ∷ ∀ a. M.Pos → Parser a → Parser a
withIndentation level = local (O.set #indentation level)
{-# INLINE withIndentation #-}

-- | Tell a parser to stop consuming junk when hitting a certain element.
alsoStopOn ∷ ∀ a. (LabelledParser (), LabelledParser ()) → Parser a → Parser a
alsoStopOn (pre, post) = alsoStopOnPre pre . alsoStopOnPost post
{-# INLINE alsoStopOn #-}

-- | Tell a parser to stop consuming junk when hitting a certain element.
--
-- The given parser runs before the one we're actively trying to complete.
alsoStopOnPre ∷ ∀ stop a. LabelledParser stop → Parser a → Parser a
alsoStopOnPre pre = local $ O.over (#stopOn % O._1) (keyedAppend . fmap void $ pre)
{-# INLINE alsoStopOnPre #-}

-- | Tell a parser to stop consuming junk when hitting a certain element.
--
-- The given parser runs before the one we're actively trying to complete.
alsoStopOnPost ∷ ∀ stop a. LabelledParser stop → Parser a → Parser a
alsoStopOnPost post = local $ O.over (#stopOn % O._2) (keyedAppend . fmap void $ post)
{-# INLINE alsoStopOnPost #-}

-- | Tells a parser to only stop on bad indentation & EOF.
resetStopOn ∷ ∀ a. Parser a → Parser a
resetStopOn =
  local $
    O.set
      #stopOn
      (KeyedParser mempty badIndent, KeyedParser mempty M.eof)
{-# INLINE resetStopOn #-}

-- | Create a custom error. The offset is there for sorting purposes.
reportError
  ∷ Text
  → Error.Doc
  → [(Error.Span, DG.Marker Error.Doc)]
  → [DG.Note Error.Doc]
  → Parser ()
reportError code desc markers hints = do
  offset ← M.getOffset
  M.registerFancyFailure
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
  let initialCtx =
        ParserContext
          (M.mkPos 1)
          ( KeyedParser mempty empty
          , KeyedParser mempty empty
          )

  let initialState = ParserState mempty IAny

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

-- | Forget every hint collected by megaparsec.
--
-- Used for performance only.
noHints ∷ ∀ a. Parser a → Parser a
noHints p = ReaderT \ctx → StateT \s →
  MI.ParsecT \ps cok cerr eok eerr →
    MI.unParser
      (runStateT (runReaderT p ctx) s)
      ps
      (\a st _ → cok a st mempty)
      (\pe st → cerr pe st)
      (\a st _ → eok a st mempty)
      (\pe st → eerr pe st)
{-# INLINE noHints #-}

runParser
  ∷ ∀ a
   . M.Parsec Error Text a
  → String
  → Text
  → ([Error.Report], Maybe a)
runParser p s i = runIdentity do
  (MI.Reply s' _ result) ← MI.runParsecT p (M.initialState s i)
  -- TODO: take custom error spans into account when sorting
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
          [ (iSpan actual, DG.This $ "Actual indentation")
          , (iSpan expected, DG.Where $ "Expected indentation")
          ]
          [shouldNeverSeeThis]
   where
    -- The span for an indentation error
    iSpan ∷ M.Pos → Base.Span
    iSpan pos =
      O.set (#end % O._2) (M.unPos pos + 1)
        . O.set (#begin % O._2) (M.unPos pos)
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
          , fold
              . intersperse ", "
              . fmap prettyItem
              $ Set.toList variants
          , "."
          ]

  prettyItem = PP.dquotes . PP.pretty . M.showErrorItem (Proxy @Text)

reportExpectedError ∷ Error.Span → Text → Parser ()
reportExpectedError s l = do
  reportError
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
{-# INLINE spanned #-}

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
  void $ checkIndentation True
  (innerSpan, inner) ← spanned p
  O.assign #relation IGT

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
checkIndentation ∷ Bool → Parser (Bool, M.Pos)
checkIndentation shouldReport = do
  col ← O.view #sourceColumn <$> M.getSourcePos
  indentation ← O.gview #indentation
  relation ← O.use #relation
  ok ← case relation of
    IEQ | col /= indentation → do
      when shouldReport do
        M.registerFancyFailure . Set.singleton $
          M.ErrorIndentation EQ indentation col
      pure False
    IGT | col <= indentation → do
      when shouldReport do
        M.registerFancyFailure . Set.singleton $
          M.ErrorIndentation GT indentation col
      pure False
    IGTE | col < indentation → do
      -- TODO: this reporting is just wrong
      when shouldReport do
        M.registerFancyFailure . Set.singleton $
          M.ErrorIndentation GT indentation col
      pure False
    _ → pure True
  pure (ok, col)

-- | Parse space characters / trivia. Although it might look at it, not all
-- results are thrown away. Parsed trivia are saved in the parser state.
sc ∷ Parser ()
sc =
  M.skipMany . M.choice $
    [ M.hidden M.space1
    , M.hidden comment
    ]
{-# INLINE sc #-}

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
mkBlock ∷ Parser BlockMaker
mkBlock =
  checkIndentation True <&> \(_, expected) → BlockMaker \p → do
    O.assign #relation IEQ
    r ← withIndentation expected p
    O.assign #relation IGT
    pure r

-- | Similar to @mkBlock@, except returning @Nothing@ on indentation violations.
tryMkBlock ∷ Parser (Maybe BlockMaker)
tryMkBlock =
  checkIndentation False <&> \(s, expected) →
    guard s $> BlockMaker \p → do
      O.assign #relation IEQ
      r ← withIndentation expected p
      O.assign #relation IGT
      pure r

blockLike ∷ ∀ a. BlockMaker → Parser a → Parser a
blockLike (BlockMaker f) a = f a

-- | Turns a parser into one with strict indentation enabled. Check
-- `ParserState.relation` for details regarding what this means.
exactlyIndented ∷ ∀ a. Parser a → Parser a
exactlyIndented p =
  -- TODO: make this not report indentation errors
  mkBlock >>= flip blockLike p

restoreRelation ∷ ∀ a. Parser a → Parser a
restoreRelation p = do
  r ← O.use #relation
  result ← p
  O.assign #relation r
  pure result

-- | Make a parser skip indentation checks (kinda...).
anyIndentation ∷ ∀ a. Parser a → Parser a
anyIndentation p = restoreRelation do
  O.assign #relation IAny *> p

-- | Make a parser only work at the very start of the line.
unindented ∷ ∀ a. Parser a → Parser a
unindented p = restoreRelation do
  O.assign #relation IEQ *> withIndentation (M.mkPos 1) p

-- | A parser which succeeds precisely when the indentation rules are violated.
badIndent ∷ Parser ()
badIndent = do
  (ok, _) ← checkIndentation False
  when ok $ fail "bad indent"
{-# INLINE badIndent #-}

---------- Keyed parsers
data KeyedParser a = KeyedParser (Set Text) (Parser a)
  deriving (Generic, Functor)

keyedAppend ∷ ∀ a. LabelledParser a → KeyedParser a → KeyedParser a
keyedAppend (l, p) k@(KeyedParser ls ps)
  | Set.member l ls = k
  | otherwise = KeyedParser (Set.insert l ls) (ps <|> p)

---------- Labelled parsers

-- | Like @'Parser', but contains an additional label for errors.
type LabelledParser a = (Text, Parser a)

label ∷ ∀ a. Text → Parser a → LabelledParser a
label l p = (l, p)
{-# INLINE label #-}

---------- Error-tolerant parsing

-- | Consume text, annotating it as junk, until the given parser succeeds.
junkTill ∷ ∀ a. (Base.HasTrivia a) ⇒ LabelledParser a → Parser a
junkTill (l, p) = do
  triviaAmount ← length <$> O.use #trivia
  (chunks, res) ← M.manyTill_ single p

  case nonEmpty chunks of
    Just neChunks → do
      let chunksSpan = foldl1 Error.mergeSpans $ fst <$> neChunks
      let piece = Base.TJunk chunksSpan (foldMap snd chunks)
      reportError
        "Junk"
        ( PP.hsep
            [ "I was looking for a"
            , PP.pretty l
            , "when I came across a bunch of unreadable characters."
            ]
        )
        [(chunksSpan, DG.This "I have no idea what this is supposed to mean.")]
        []

      case Base.attachTrivia (pure piece) res of
        Just res' → pure res'
        Nothing → do
          O.modifying #trivia (Seq.insertAt triviaAmount piece)
          pure res
    Nothing → pure res
 where
  single ∷ Parser (Error.Span, Text)
  single = spanned chunk <* sc

  chunk ∷ Parser Text
  chunk = do
    h ← M.anySingle
    if Char.isAlpha h
      then do
        more ← M.takeWhileP (Just "junk") Char.isAlpha
        pure $ Text.singleton h <> more
      else pure $ Text.singleton h

-- | Similar to 'junkTill', except will automatically stop when encountering
-- an element matching the context's 'stopOn' parser.
tryJunkTill ∷ ∀ a. (Base.HasTrivia a) ⇒ LabelledParser a → Parser (Maybe a)
tryJunkTill (l, inner) = do
  (KeyedParser _ pre, KeyedParser _ post) ← O.gview #stopOn
  junkTill
    . (l,)
    . M.choice
    $ [ Nothing <$ M.lookAhead pre
      , Just <$> inner
      , Nothing <$ M.lookAhead post
      ]
{-# INLINE tryJunkTill #-}

-- | Similar to @'tryJunkTill', except an error gets reported if the value
-- is missing.
tryReportedJunkTill ∷ ∀ a. (Base.HasTrivia a) ⇒ LabelledParser a → Parser (Maybe a)
tryReportedJunkTill p = do
  (resultSpan, result) ← spanned $ tryJunkTill p
  when (isNothing result) do
    reportExpectedError resultSpan $ fst p

  pure result

---------- Pretty printing
instance PP.Pretty ParserState where
  pretty st =
    Base.prettyTree
      "ParserState"
      [ PP.pretty ("relation" ∷ Text, O.view #relation st)
      , PP.pretty ("trivia" ∷ Text, toList $ O.view #trivia st)
      ]

instance PP.Pretty IndentationRelation where
  pretty i = PP.pretty (show i ∷ Text)
