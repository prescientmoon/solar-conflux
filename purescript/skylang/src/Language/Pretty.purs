module Sky.Language.Pretty where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam)
import Data.Array (foldl)
import Data.Array as Array
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi (bold, foreground, italic)
import Dodo.Common (jsParens)
import Safe.Coerce (coerce)
import Sky.Language.Error (class SourceSpot, SKY_ERROR, lambdaArgument, nameOfLet, piArgument)
import Sky.Language.Eval (QUOTATION_ENV, quote)
import Sky.Language.PrettyM (PrintM, extendPrintScope, getScope, generateMetaName, lookupSource)
import Sky.Language.Term (Index(..), Term(..), Value)
import Type.Row (type (+))

---------- Types
data SkyLog a
  = Checking (Doc a) (Doc a)
  | Inferring (Doc a)
  | Unifying (Doc a) (Doc a)

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
prettyPrintValue
  :: forall a r
   . SourceSpot a
  => Value a
  -> PrintM a (QUOTATION_ENV + SKY_ERROR a r) (Doc GraphicsParam)
prettyPrintValue value = quote value >>= prettyPrintTerm

prettyPrintTerm :: forall a r. SourceSpot a => Term a -> PrintM a r (Doc GraphicsParam)
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

    innermostDoc <- extendPrintScope argNames $ prettyPrintTerm innermost
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
      previous <- extendPrintScope [ name ] previous
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
    name <- generateMetaName meta
    in foreground Green $ Dodo.text $ "?" <> name
  InsertedMeta _ meta _ -> ado
    name <- generateMetaName meta
    in foreground Green $ Dodo.text $ "???" <> name
  Pi source domain codomain -> do
    name <- lookupSource (piArgument source)
    domain <- prettyPrintTerm domain
    codomain <- extendPrintScope [ name ] $ prettyPrintTerm codomain
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

