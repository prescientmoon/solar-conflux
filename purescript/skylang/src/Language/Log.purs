module Sky.Language.Log where

import Prelude

import Ansi.Codes (GraphicsParam)
import Data.Array (fold)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZA
import Debug (spy, trace, traceM)
import Dodo (Doc)
import Dodo as Doc
import Dodo as Dodo
import Dodo.Ansi (bold, italic)
import Run (Run)
import Run as Run
import Run.State (STATE)
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Run.Writer as Writer
import Sky.Language.Ast (Ast)
import Sky.Language.Effects (ActionId(..), SKY_LOGGER, SkyAction(..), SkyLog(..), SkyM, _skyLogger)
import Sky.Language.Error (class SourceSpot)
import Sky.Language.Pretty (errorish, prettyPrintAst, prettyPrintTerm, prettyPrintValue)
import Sky.Language.Term (Term, Value)
import Type.Row (type (+))

-- | Internal helper for logging actions
logAction
  :: forall o r
   . SkyAction GraphicsParam
  -> (o -> Run (SKY_LOGGER + SUPPLY Int r) (Doc GraphicsParam))
  -> Run (SKY_LOGGER + SUPPLY Int r) o
  -> Run (SKY_LOGGER + SUPPLY Int r) o
logAction action prettify compute = do
  id <- map ActionId Supply.generate
  Writer.tellAt _skyLogger [ Started id action ]
  result <- compute
  prettyResult <- prettify result
  Writer.tellAt _skyLogger [ Finished id prettyResult ]
  pure result

-- | I HATE CYCLING IMPORTS SO MUCH
type QuoteImplementation a r = Value a -> SkyM a r (Term a)

unifying
  :: forall a r
   . SourceSpot a
  => Value a
  -> Value a
  -> SkyM a r Unit
  -> SkyM a r Unit
unifying lhs rhs compute = do
  lhsDoc <- prettyPrintValue lhs
  rhsDoc <- prettyPrintValue rhs
  logAction (Unifying lhsDoc rhsDoc) printOutput compute
  where
  printOutput _ = pure $ Doc.text "Unification finished with no issues!"

checking
  :: forall a r
   . SourceSpot a
  => Ast a
  -> Value a
  -> SkyM a r (Term a)
  -> SkyM a r (Term a)
checking lhs rhs compute = do
  lhsDoc <- prettyPrintAst lhs
  rhsDoc <- prettyPrintValue rhs
  logAction (Checking lhsDoc rhsDoc) prettyPrintTerm compute

evaluating
  :: forall a r
   . SourceSpot a
  => Term a
  -> SkyM a r (Value a)
  -> SkyM a r (Value a)
evaluating term compute = compute
  where
  a = do
    termDoc <- prettyPrintTerm term
    logAction (Evaluating termDoc) prettyPrintValue compute

inferring
  :: forall a r
   . SourceSpot a
  => Ast a
  -> SkyM a r (Term a /\ Value a)
  -> SkyM a r (Term a /\ Value a)
inferring ast compute = do
  doc <- prettyPrintAst ast
  logAction (Inferring doc) prettyOutput compute
  where
  prettyOutput (term /\ inferred) = ado
    term <- prettyPrintTerm term
    inferred <- prettyPrintValue inferred
    in
      fold
        [ info "The ast has been normalized to"
        , Dodo.spaceBreak
        , term
        , Dodo.break
        , info "and has type"
        , Dodo.spaceBreak
        , inferred
        ]

---------- Pretty printing helpers
info :: String -> Doc GraphicsParam
info = Dodo.text >>> italic >>> bold

---------- Pretty printing of logs
-- | Monad pretty printing of logs takes place in
type LogPrintingM = Run (STATE (Maybe (ZipperArray SkyLog)) ())

-- | Advance the log-reading "cursor" by 1 (does nothing if the input is already over)
advanceLogCursor :: LogPrintingM Unit
advanceLogCursor = void $
  State.get >>= traverse \logs -> do
    State.put $ ZA.goNext logs

-- | Get the log the cursor is currently hovering over
currentLog :: LogPrintingM (Maybe SkyLog)
currentLog =
  State.get <#> map \logs ->
    ZA.current logs

-- | Pretty print an array of logs
prettyPrintLogArray :: Array SkyLog -> Doc GraphicsParam
prettyPrintLogArray array = prettyPrintLogs
  # map (Array.intercalate Dodo.break)
  # State.evalState (ZA.fromArray array)
  # Run.extract

prettyPrintLogs :: LogPrintingM (Array (Doc GraphicsParam))
prettyPrintLogs = do
  current <- currentLog
  case current of
    Just (Started id _) -> do
      traceM $ "Opening " <> show id
      doc <- prettyPrintFirstLog
      docs <- prettyPrintLogs
      pure $ Array.cons doc docs
    Just (Finished id _) -> do
      traceM $ "Ending " <> show id
      pure []
    _ -> pure []

-- other -> do
--   advanceLogCursor
--   prettyPrintLogs

prettyPrintFirstLog :: LogPrintingM (Doc GraphicsParam)
prettyPrintFirstLog = do
  current <- currentLog
  case current of
    Nothing -> pure $ errorish "Impossible situation"
    Just (Finished _ _) -> pure $ errorish "Impossible situation"
    Just (Started id action) -> do
      advanceLogCursor
      midDoc <- prettyPrintLogs <#> Array.intercalate (Dodo.break)
      endDoc <- currentLog
        >>= case _ of
          Just (Finished finishedId endDoc) | finishedId == id -> ado
            advanceLogCursor
            in Just endDoc
          _ -> pure Nothing
      pure $ Array.fold
        [ startDoc action
        , Dodo.break
        , Dodo.indent midDoc
        , Dodo.break
        , case endDoc of
            Nothing -> errorish "An error occured"
            Just endDoc -> processedEndDoc action endDoc
        ]
  where
  processedEndDoc starting endDoc = case starting of
    Inferring _ -> endDoc
    Checking _ _ -> Array.fold
      [ info "The ast has been elaborated into"
      , Dodo.spaceBreak
      , Dodo.indent endDoc
      ]
    Evaluating _ -> Array.fold
      [ info "The term has evluated to"
      , Dodo.spaceBreak
      , Dodo.indent endDoc
      ]
    _ -> Dodo.text "Not implemented"
  startDoc = case _ of
    Inferring v -> Array.fold
      [ info "Inferring the type of value"
      , Dodo.spaceBreak
      , Dodo.indent v
      ]
    Checking t v -> Array.fold
      [ info "Checking whether value"
      , Dodo.spaceBreak
      , Dodo.indent t
      , Dodo.spaceBreak
      , info "has type"
      , Dodo.spaceBreak
      , Dodo.indent v
      ]
    Evaluating term -> Array.fold
      [ info "Evaluating term"
      , Dodo.spaceBreak
      , Dodo.indent term
      ]
    _ -> Dodo.text "Not implemented"
