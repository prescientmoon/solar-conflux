module Sky.Language.Pretty where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam)
import Data.Array (foldl)
import Data.Array as Array
import Data.Foldable (fold, intercalate)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.String.CodeUnits as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi (bold, foreground, italic)
import Dodo.Common (jsParens)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Run (Run)
import Run as Run
import Run.Reader (READER)
import Run.Reader as Reader
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Safe.Coerce (coerce)
import Sky.Language.Cst (AbstractSourceLocation, SourcePosition(..), SourceSpan(..), SourceMap)
import Sky.Language.Error (lambdaArgument, nameOfLet, piArgument)
import Sky.Language.Term (Index(..), MetaVar, Term(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

---------- Effect stuff
newtype PrintContext = PrintContext
  { metaNames :: HashMap MetaVar String
  , scope :: Array String
  , sourceMap :: SourceMap
  , originalText :: String
  }

-- | Monad carrying a print context
type PrintM r = Run (SUPPLY Int + READER PrintContext r)

-- | Expose the current scope
getScope :: forall r. PrintM r (Array String)
getScope = Reader.ask <#> \(PrintContext { scope }) -> scope

-- | Expose the current source map
getSourceMap :: forall r. PrintM r SourceMap
getSourceMap = Reader.ask <#> \(PrintContext { sourceMap }) -> sourceMap

lookupSpan :: forall r. SourceSpan -> PrintM r (Maybe String)
lookupSpan (SourceSpan { start: SourcePosition from, end: SourcePosition to }) =
  Reader.ask <#> \(PrintContext { originalText }) ->
    String.slice from.index (to.index - from.index) originalText

---------- Add a bunch of variables in scope 
extendScope :: forall r. Array String -> PrintM r ~> PrintM r
extendScope with = Reader.local $ Newtype.over PrintContext
  $ Record.modify _scope \a -> with <> a

generateVar :: forall r. PrintM r String
generateVar = ado
  id <- Supply.generate
  in "_" <> show id

lookupMeta :: forall r. MetaVar -> PrintM r String
lookupMeta meta = Reader.ask >>= \(PrintContext { metaNames }) ->
  case HM.lookup meta metaNames of
    Nothing -> generateVar
    Just a -> pure a

lookupSource :: forall r. AbstractSourceLocation -> PrintM r String
lookupSource source = do
  sourceMap <- getSourceMap
  case HM.lookup source sourceMap of
    Nothing -> generateVar
      where
      a = unsafePerformEffect $ log $ unsafeCoerce "oof"
    -- b = unsafePerformEffect $ log $ unsafeCoerce sourceMap
    Just span -> lookupSpan span >>= case _ of
      Nothing -> generateVar
      Just name -> pure name

runPrintM :: forall a. PrintContext -> PrintM () a -> a
runPrintM ctx = Reader.runReader ctx
  >>> Supply.runSupply ((+) 1) 0
  >>> Run.extract

---------- Helpers
-- | Error-like looking output
errorish :: String -> Doc GraphicsParam
errorish = foreground Red <<< bold <<< Dodo.text

-- | Keyword-like looking text
keyword :: String -> Doc GraphicsParam
keyword = Dodo.text >>> foreground Yellow >>> italic

-- | Surround a doc by parenthesis when a condition is true
parensWhen :: forall a. Boolean -> Doc a -> Doc a
parensWhen false = identity
parensWhen true = jsParens

---------- Implementations

prettyPrintTerm :: forall r. Term AbstractSourceLocation -> PrintM r (Doc GraphicsParam)
prettyPrintTerm = case _ of
  Star _ -> pure $ foreground Yellow $ Dodo.text "*"
  Var source index -> ado
    scope <- getScope
    in
      case Array.index scope (coerce index) of
        Just name -> Dodo.text name
        Nothing -> errorish "<Variable not in scope>"
  Assumption _ inner -> ado
    innerDoc <- prettyPrintTerm inner
    in
      fold
        [ keyword "assume"
        , Dodo.space
        , parensWhen (needsParens inner) innerDoc
        ]
    where
    needsParens = case _ of
      SourceAnnotation _ a -> needsParens a
      Application _ _ _ -> true
      Pi _ _ _ -> true
      _ -> false
  SourceAnnotation _ a -> prettyPrintTerm a
  lam@(Lambda _ _) -> do
    argNames <- for args \arg -> lookupSource (lambdaArgument arg)

    innermostDoc <- extendScope argNames $ prettyPrintTerm innermost
    pure $ fold
      [ lambda
      , Dodo.text (intercalate " " argNames)
      , Dodo.space
      , arrow
      , Dodo.space
      , innermostDoc
      ]
    where
    innermost /\ args = collectLambdas lam

  let_@(Let _ _ _) -> do
    names <- for definitions \(name /\ value) ->
      Tuple value <$> lookupSource (nameOfLet name)
    let
      innermostDoc = ado
        innerDoc <- prettyPrintTerm innermost
        in keyword "in" <> Dodo.space <> innerDoc
    doc <- foldl go innermostDoc names
    pure $ keyword "let" <> Dodo.break <> doc
    where
    innermost /\ definitions = collectLets let_
    go previous (value /\ name) = do
      previous <- extendScope [ name ] previous
      value <- prettyPrintTerm value
      pure $ fold
        [ Dodo.indent
            $ fold
                [ Dodo.text name
                , Dodo.space
                , equal
                , value
                ]
        , Dodo.break
        , previous
        ]
  Meta _ meta -> ado
    name <- lookupMeta meta
    in foreground Green $ Dodo.text $ "?" <> name
  InsertedMeta _ meta _ -> ado
    name <- lookupMeta meta
    in foreground Green $ Dodo.text $ "???" <> name
  Pi source domain codomain -> do
    name <- lookupSource (piArgument source)
    domain <- prettyPrintTerm domain
    codomain <- extendScope [ name ] $ prettyPrintTerm codomain
    pure case name of
      "_" -> fold
        [ jsParens domain
        , Dodo.space
        , arrow
        , Dodo.space
        , codomain
        ]
      _ -> fold
        [ jsParens $ fold
            [ Dodo.text name
            , doubleColon
            , Dodo.space
            , domain
            ]
        , Dodo.space
        , arrow
        , Dodo.space
        , codomain
        ]
  Application _ lhs rhs -> ado
    lhsDoc <- prettyPrintTerm lhs
    rhsDoc <- prettyPrintTerm rhs
    in
      fold
        [ parensWhen (lhsNeedsParens lhs) lhsDoc
        , Dodo.space
        , parensWhen (rhsNeedsParens rhs) rhsDoc
        ]
    where
    rhsNeedsParens = case _ of
      SourceAnnotation _ inner -> rhsNeedsParens inner
      Pi _ _ _ -> true
      Assumption _ _ -> true
      Application _ _ _ -> true
      _ -> false
    lhsNeedsParens = case _ of
      SourceAnnotation _ inner -> lhsNeedsParens inner
      Lambda _ _ -> true
      Pi _ _ _ -> true
      Let _ _ _ -> true
      Assumption _ _ -> true
      _ -> false

collectLambdas :: forall a. Term a -> (Term a /\ Array a)
collectLambdas (Lambda source body) = innermost /\ (args <> [ source ])
  where
  innermost /\ args = collectLambdas body
collectLambdas a = a /\ []

collectLets :: forall a. Term a -> (Term a /\ Array (a /\ Term a))
collectLets (Let source value body) = innermost /\
  (args <> [ source /\ value ])
  where
  innermost /\ args = collectLets body
collectLets a = a /\ []

---------- Constants
lambda :: Doc GraphicsParam
lambda = foreground Blue $ bold $ Dodo.text "λ"

equal :: Doc GraphicsParam
equal = foreground Blue $ bold $ Dodo.text "="

arrow :: Doc GraphicsParam
arrow = foreground Blue $ bold $ Dodo.text "->"

doubleColon :: Doc GraphicsParam
doubleColon = foreground Blue $ bold $ Dodo.text ":"

---------- Typeclass instances
derive instance Newtype PrintContext _

---------- Proxies
_scope :: Proxy "scope"
_scope = Proxy