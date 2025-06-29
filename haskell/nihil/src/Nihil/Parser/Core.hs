module Nihil.Parser.Core
  ( Parser
  , string
  , token
  , parseTest
  , withIndentation
  , exactlyIndented
  , blockLike
  , name
  , badIndent
  , loose2
  , junkTill
  ) where

import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Sequence ((|>))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Nihil.Cst.Base qualified as Base
import Optics qualified as O
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Debug qualified as M
import Text.Megaparsec.Internal qualified as MI
import Text.Megaparsec.State qualified as M
import Text.Pretty.Simple (pPrint)

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
  , stopOn ∷ Parser ()
  -- ^ When consuming junk input, stop when this parser succeeds
  }
  deriving (Generic)

type Parser = ReaderT ParserContext (StateT ParserState (M.Parsec Void Text))

-- | Save a piece of trivia to the parser state. The piece will later on get
-- associated with the next token, and kept inside the CST.
saveTrivia ∷ Base.Trivia → Parser ()
saveTrivia c = O.modifying #trivia (|> c)

-- | Update the minimum indentation of some parser.
withIndentation ∷ ∀ a. M.Pos → Parser a → Parser a
withIndentation level = local (O.set #indentation level)

-- | Attempt to run a parser on some string, reporting the results to stdout.
-- An error is reported if the parser does not consume the full string it's
-- given. The parser state at the very end of the parse is also shown.
parseTest ∷ ∀ a. (Show a) ⇒ Parser a → Text → IO ()
parseTest p input = do
  let initialCtx = ParserContext (M.mkPos 1) empty
  let initialState = ParserState mempty True

  let readerResult = runReaderT (p <* M.eof) initialCtx
  let stateResult = runStateT readerResult initialState
  let (errs, result) = runParser stateResult "" input

  for_ result \(x, finalState) → do
    pPrint x
    pPrint finalState

  for_ (NE.nonEmpty errs) \ne → do
    putStr . M.errorBundlePretty $
      M.ParseErrorBundle
        { bundleErrors = ne
        , bundlePosState = M.statePosState $ M.initialState "" input
        }

runParser
  ∷ ∀ a
   . M.Parsec Void Text a
  → String
  → Text
  → ([M.ParseError Text Void], Maybe a)
runParser p s i = runIdentity do
  (MI.Reply s' _ result) ← MI.runParsecT p (M.initialState s i)
  let bundle es = NE.toList $ NE.sortWith M.errorOffset es
  pure $ case result of
    MI.OK _ x →
      case NE.nonEmpty (M.stateParseErrors s') of
        Nothing → ([], Just x)
        Just de → (bundle de, Just x)
    MI.Error e →
      ([e], Nothing)

---------- Base parsers
string ∷ Text → Parser (Base.Token ())
string = token . void . M.string

name ∷ Parser Base.Name
name = M.label "name" $ token $ M.try do
  -- TODO: support more chars
  let chunk = M.takeWhile1P (Just "character") \c → Char.isAlphaNum c
  let sep = M.string "."
  result ← Text.intercalate "." <$> M.sepBy1 chunk sep
  when (elem result illegal) $ do
    fail $ Text.unpack result <> " is an illegal name"
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

---------- Core combinators

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

-- | Turn a parser into one that keeps track of the source span it's consumed.
spanned ∷ ∀ a. Parser a → Parser (Base.Span, a)
spanned p = do
  start ← M.getSourcePos
  inner ← p
  end ← M.getSourcePos
  pure (Base.Span start end, inner)

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

-- | Creates a parsers for a series of things that are aligned to eachother
-- horizontally. Examples include:
-- - pattern match branches
-- - statements in some block
-- - declarations in a module
blockLike ∷ ∀ a. Parser a → Parser (Parser a)
blockLike p = do
  expected ← checkIndentation
  pure do
    O.assign #exactIndentation True
    r ← withIndentation expected p
    O.assign #exactIndentation False
    pure r

-- | Turns a parser into one with strict indentation enabled. Check
-- `ParserState.exactIndentation` for details regarding what this means.
exactlyIndented ∷ ∀ a. Parser a → Parser a
exactlyIndented = join . blockLike

-- | A parser which succeeds precisely when the indentation rules are violated.
badIndent ∷ Parser ()
badIndent = M.eof <|> M.notFollowedBy (void checkIndentation)

data Loose a b = LFirst a | LSecond b | LNeither
  deriving (Generic, Show)

-- | The base building block of error-tolerant parsing. Ideally, this parser
-- would simply run the two given parsers in sequence. In reality, the input
-- might at times be incomplete, so this parser eats up junk input, and returns
-- the partial results of the two parsers.
loose2 ∷ ∀ a b. (Text, Parser a) → (Text, Parser b) → Parser (Maybe a, Maybe b)
loose2 (la, pa) (lb, pb) = do
  stopOn ← O.gview #stopOn
  let neither = badIndent <|> stopOn
  offset ← M.getOffset

  end ←
    junkTill . M.choice $
      [ LNeither <$ neither
      , LSecond <$> pb
      , LFirst <$> pa
      ]

  case end of
    LNeither → M.region (M.setErrorOffset offset) do
      M.registerFancyFailure . Set.singleton $
        M.ErrorFail . Text.unpack $
          "Expected " <> la
      pure (Nothing, Nothing)
    LSecond b → do
      M.region (M.setErrorOffset offset) $
        M.registerFancyFailure . Set.singleton $
          M.ErrorFail . Text.unpack $
            "Expected " <> la
      pure (Nothing, Just b)
    LFirst a → do
      offset' ← M.getOffset
      end' ← junkTill $ (Left <$> neither) <|> (Right <$> pb)
      case end' of
        Left _ → do
          M.region (M.setErrorOffset offset') $
            M.registerFancyFailure . Set.singleton $
              M.ErrorFail . Text.unpack $
                "Expected " <> lb
          pure (Just a, Nothing)
        Right b → pure (Just a, Just b)

-- | Consume text, annotating it as junk, until the given parser succeeds.
junkTill ∷ ∀ a. Parser a → Parser a
junkTill p = do
  void checkIndentation
  let single = M.anySingle <* sc <* O.assign #exactIndentation False
  (charsSpan, chars) ← spanned $ M.manyTill single (M.lookAhead p)
  unless (null chars) do
    saveTrivia $ Base.TJunk charsSpan (Text.pack chars)
  p
