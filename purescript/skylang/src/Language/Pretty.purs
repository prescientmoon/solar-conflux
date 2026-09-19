module Sky.Language.Pretty where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam)
import Data.Array (foldl)
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi (bold, foreground, italic)
import Dodo.Common (jsParens)
import Safe.Coerce (coerce)
import Sky.Language.Ast (Ast(..))
import Sky.Language.Effects (PrintM, extendPrintScope, generateMetaName, getScope, lookupSource)
import Sky.Language.Error (class SourceSpot, lambdaArgument, nameOfLet, piArgument)
import Sky.Language.Term (Closure(..), Index(..), Level(..), Name(..), NameEnv(..), Spine(..), Term(..), Value(..))

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

-- | Print a meta variable
printMeta :: String -> Doc GraphicsParam
printMeta name = foreground Green $ Dodo.text $ "?" <> name

---------- Implementations
-- prettyPrintValue
--   :: forall a r
--    . SourceSpot a
--   => (Value a -> PrintM a (QUOTATION_ENV + SKY_ERROR a r) (Term a))
--   -> Value a
--   -> PrintM a (QUOTATION_ENV + SKY_ERROR a r) (Doc GraphicsParam)
-- prettyPrintValue quote value = quote value >>= prettyPrintTerm

prettyPrintValue
  :: forall a r
   . SourceSpot a
  => Value a
  -> PrintM a r (Doc GraphicsParam)
prettyPrintValue = case _ of
  VStar _ -> pure star
  VSourceAnnotation _ inner -> prettyPrintValue inner
  VVariableApplication _ level spine -> do
    NameEnv scope <- getScope
    let
      varDoc = case Array.index scope (Array.length scope - 1 - coerce level) of
        Just name -> Dodo.text name
        Nothing -> errorish "<Variable not in scope>"
    printSpineApplication varDoc spine
  VMetaApplication _ meta spine -> do
    name <- generateMetaName meta
    printSpineApplication (printMeta name) spine
  VAssumptionApplication _ type_ spine -> do
    typeDoc <- prettyPrintValue type_
    let
      doc = jsParens $ Array.fold
        [ keywordAssume
        , Dodo.space
        , parensWhen (needsParens type_) typeDoc
        ]
    printSpineApplication doc spine
    where
    needsParens = case _ of
      VMetaApplication _ _ (Spine []) -> false
      VMetaApplication _ _ _ -> true
      VVariableApplication _ _ (Spine []) -> false
      VVariableApplication _ _ _ -> true
      VPi _ _ _ -> true
      _ -> false
  VPi source domain codomain -> do
    name <- lookupSource (piArgument source)
    domainDoc <- prettyPrintValue domain
    codomain <-
      prettyPrintClosure name codomain
    pure case name of
      "_" -> fold
        [ parensWhen (domainNeedsParens domain) domainDoc
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
            , domainDoc
            ]
        , Dodo.space
        , arrow
        , Dodo.space
        , codomain
        ]
    where
    domainNeedsParens = case _ of
      VSourceAnnotation _ a -> domainNeedsParens a
      VPi _ _ _ -> true
      VLambda _ _ -> true
      _ -> false
  VLambda source inner -> do
    name <- lookupSource (lambdaArgument source)
    innerDoc <- prettyPrintClosure name inner
    pure $ fold
      [ lambda
      , Dodo.text name
      , Dodo.space
      , arrow
      , Dodo.space
      , innerDoc
      ]
  where
  prettyPrintClosure name (Closure { term, names }) =
    extendPrintScope (Array.cons name $ coerce names) $
      prettyPrintTerm term

  printSpineApplication lhsDoc (Spine spine) =
    Array.foldM printApplication lhsDoc spine

  printApplication lhsDoc rhs = ado
    rhsDoc <- prettyPrintValue rhs
    in
      fold
        [ lhsDoc
        , Dodo.space
        , parensWhen (rhsNeedsParens rhs) rhsDoc
        ]
    where
    rhsNeedsParens = case _ of
      VPi _ _ _ -> true
      VAssumptionApplication _ _ _ -> true
      VVariableApplication _ _ (Spine []) -> false
      VVariableApplication _ _ _ -> true
      VMetaApplication _ _ (Spine []) -> false
      VMetaApplication _ _ _ -> true
      _ -> false

prettyPrintAst :: forall a r. Ast a -> PrintM a r (Doc GraphicsParam)
prettyPrintAst = case _ of
  EStar _ -> pure star
  EVar _ name -> pure $ Dodo.text $ coerce name
  EHole _ name -> pure $ printMeta (maybe "" coerce name)
  EAssumption _ inner -> ado
    innerDoc <- prettyPrintAst inner
    in
      fold
        [ keywordAssume
        , Dodo.space
        , parensWhen (needsParens inner) innerDoc
        ]
    where
    needsParens = case _ of
      EAnnotation _ _ _ -> true
      EApplication _ _ _ -> true
      EPi _ _ _ _ -> true
      _ -> false

  lam@(ELambda _ _ _) -> do
    innermostDoc <- prettyPrintAst innermost
    pure $ fold
      [ lambda
      , Dodo.text (intercalate " " (coerce args :: Array String))
      , Dodo.space
      , arrow
      , Dodo.space
      , innermostDoc
      ]
    where
    innermost /\ args = collectAstLambdas lam
  let_@(ELet _ _ _ _) -> do
    definitions <- for definitions $ traverse case _ of
      EAnnotation _ value type_ -> ado
        value <- prettyPrintAst value
        type_ <- prettyPrintAst type_
        in value /\ Just type_
      other -> ltraverse prettyPrintAst (other /\ Nothing)
    innermostDoc <- ado
      innerDoc <- prettyPrintAst innermost
      in keyword "in" <> Dodo.space <> innerDoc
    let definitionDocs = Array.reverse $ map go definitions
    pure $ fold
      [ keyword "let"
      , Dodo.break
      , Dodo.indent $ intercalate (Dodo.break <> Dodo.break) definitionDocs
      , Dodo.break
      , innermostDoc
      ]
    where
    innermost /\ definitions = collectAstLets let_
    go (name /\ value /\ annotation) = do
      let
        annotationDoc = case annotation of
          Nothing -> []
          Just annotation ->
            [ Dodo.text (coerce name)
            , Dodo.space
            , doubleColon
            , Dodo.space
            , Dodo.indent annotation
            , Dodo.break
            ]
      let
        implementation =
          [ Dodo.text (coerce name)
          , Dodo.space
          , equal
          , Dodo.space
          , Dodo.indent value
          ]

      fold
        (annotationDoc <> implementation)
  EPi _ (Name name) domain codomain -> do
    domainDoc <- prettyPrintAst domain
    codomain <- prettyPrintAst codomain
    pure case name of
      "_" -> fold
        [ parensWhen (domainNeedsParens domain) domainDoc
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
            , domainDoc
            ]
        , Dodo.space
        , arrow
        , Dodo.space
        , codomain
        ]
    where
    domainNeedsParens = case _ of
      ELet _ _ _ _ -> true
      EPi _ _ _ _ -> true
      EAnnotation _ _ _ -> true
      ELambda _ _ _ -> true
      _ -> false
  EApplication _ lhs rhs -> ado
    lhsDoc <- prettyPrintAst lhs
    rhsDoc <- prettyPrintAst rhs
    in
      fold
        [ parensWhen (lhsNeedsParens lhs) lhsDoc
        , Dodo.space
        , parensWhen (rhsNeedsParens rhs) rhsDoc
        ]
    where
    rhsNeedsParens = case _ of
      EPi _ _ _ _ -> true
      EAssumption _ _ -> true
      EApplication _ _ _ -> true
      EAnnotation _ _ _ -> true
      _ -> false
    lhsNeedsParens = case _ of
      ELambda _ _ _ -> true
      EPi _ _ _ _ -> true
      ELet _ _ _ _ -> true
      EAssumption _ _ -> true
      EAnnotation _ _ _ -> true
      _ -> false
  EAnnotation _ lhs rhs -> ado
    lhsDoc <- prettyPrintAst lhs
    rhsDoc <- prettyPrintAst rhs
    in
      fold
        [ parensWhen (lhsNeedsParens lhs) lhsDoc
        , Dodo.space
        , doubleColon <> doubleColon
        , Dodo.space
        , parensWhen (rhsNeedsParens rhs) rhsDoc
        ]
    where
    rhsNeedsParens = case _ of
      EAnnotation _ _ _ -> true
      _ -> false
    lhsNeedsParens = case _ of
      ELambda _ _ _ -> true
      EPi _ _ _ _ -> true
      ELet _ _ _ _ -> true
      EAssumption _ _ -> true
      EAnnotation _ _ _ -> true
      _ -> false

prettyPrintTerm :: forall a r. SourceSpot a => Term a -> PrintM a r (Doc GraphicsParam)
prettyPrintTerm = case _ of
  Star _ -> pure star
  Var source index -> ado
    NameEnv scope <- getScope
    in
      case Array.index scope (coerce index) of
        Just name -> Dodo.text name
        Nothing -> errorish "<Variable not in scope>"
  Assumption _ inner -> ado
    innerDoc <- prettyPrintTerm inner
    in
      fold
        [ keywordAssume
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
    in printMeta name
  InsertedMeta _ meta _ -> ado
    name <- generateMetaName meta
    in foreground Green $ Dodo.text $ "???" <> name
  Pi source domain codomain -> do
    name <- lookupSource (piArgument source)
    domainDoc <- prettyPrintTerm domain
    codomain <- extendPrintScope [ name ] $ prettyPrintTerm codomain
    pure case name of
      "_" -> fold
        [ parensWhen (domainNeedsParens domain) domainDoc
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
            , domainDoc
            ]
        , Dodo.space
        , arrow
        , Dodo.space
        , codomain
        ]
    where
    domainNeedsParens = case _ of
      SourceAnnotation _ a -> domainNeedsParens a
      Let _ _ _ -> true
      Pi _ _ _ -> true
      Lambda _ _ -> true
      _ -> false
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

---------- Helpers
-- | Base helper for implementing stuff like "collectLambdas"
collect :: forall r f. (f -> Maybe (f /\ r)) -> f -> (f /\ Array r)
collect extract = go
  where
  go term = case extract term of
    Nothing -> term /\ []
    Just (inner /\ extra) -> innermost /\ (extracted <> [ extra ])
      where
      innermost /\ extracted = go inner

collectLambdas :: forall a. Term a -> (Term a /\ Array a)
collectLambdas = collect case _ of
  -- TODO: handle source annotations
  Lambda source body -> Just (body /\ source)
  _ -> Nothing

collectAstLambdas :: forall a. Ast a -> (Ast a /\ Array Name)
collectAstLambdas = collect case _ of
  ELambda _ arg body -> Just (body /\ arg)
  _ -> Nothing

collectLets :: forall a. Term a -> (Term a /\ Array (a /\ Term a))
collectLets = collect case _ of
  Let source value body -> Just (body /\ source /\ value)
  _ -> Nothing

collectAstLets :: forall a. Ast a -> (Ast a /\ Array (Name /\ Ast a))
collectAstLets = collect case _ of
  ELet _ name value body -> Just (body /\ name /\ value)
  _ -> Nothing

---------- Constants
lambda :: Doc GraphicsParam
lambda = foreground Blue $ bold $ Dodo.text "λ"

equal :: Doc GraphicsParam
equal = foreground Blue $ bold $ Dodo.text "="

arrow :: Doc GraphicsParam
arrow = foreground Blue $ bold $ Dodo.text "->"

doubleColon :: Doc GraphicsParam
doubleColon = foreground Blue $ bold $ Dodo.text ":"

star :: Doc GraphicsParam
star = foreground Yellow $ Dodo.text "*"

keywordAssume :: Doc GraphicsParam
keywordAssume = keyword "assume"