module Moonlog.Unify where

import Prelude

import Data.Array (findMap, foldM, length, zip)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, inj)
import Moonlog.Error (WITH_ERRORS, throw)
import Moonlog.Expression (Ast(..), Expression)
import Moonlog.Parser.Cst (SourceSpan, sourceSpan)
import Moonlog.Substitution (Substitution, mergeSubstitutions, substitute)
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Type.Proxy (Proxy(..))

-- | Example: a ~ S a
type InfiniteVarError =         
    { var :: String
    , term :: Expression
    , infiniteReference :: SourceSpan }

-- | All the errors which can occur durirng unification
type UnificationErrors r =
    ( cannotUnify :: Expression /\ Expression
    , infiniteVar :: InfiniteVarError
    , differentPattern :: String /\ String
    , differentLenghts :: Array Expression /\ Array Expression
    | r )

-- | Alias to WITH_ERRORS except always set to UnificationErrors
type WITH_UNIFICTAION_ERRORS :: forall k. Row Type -> Row (k -> Type) -> Row (k -> Type)
type WITH_UNIFICTAION_ERRORS e r = WITH_ERRORS (UnificationErrors e) r

-- | Check if an expression references a variable, and if it does, return the source span where that happens
occursCheck :: String -> Expression -> Maybe SourceSpan
occursCheck target (Constructor { arguments }) = findMap (occursCheck target) arguments
occursCheck target (Var s v) | v == target = Just s
                             | otherwise = Nothing

-- | Bind a variable to a term, returning a substitution
bindVar :: forall e r. String -> Expression -> Run (WITH_UNIFICTAION_ERRORS e r) Substitution
bindVar name term 
    | Just span <- occursCheck name term 
    = throw (sourceSpan term) $ infiniteVar
        { var: name
        , infiniteReference: span
        , term }
    | otherwise = pure $ HashMap.singleton name term
        

unify :: forall e r. Expression -> Expression -> Run (WITH_UNIFICTAION_ERRORS e r) Substitution
unify (Var _ a) (Var _ b) | a == b = pure HashMap.empty 
unify (Var _ a) b = bindVar a b 
unify a (Var _ b) = bindVar b a 
unify (Constructor a) (Constructor b)
    | a.name /= b.name = throw a.span $ differentPattern a.name b.name
    | otherwise = unifyMany a.arguments b.arguments
-- unify a b = throw (sourceSpan a) (cannotUnify a b)

unifyMany :: forall e r. Array Expression -> Array Expression -> Run (WITH_UNIFICTAION_ERRORS e r) Substitution
unifyMany left right 
    | length left == length right = foldM go HashMap.empty (zip left right)
    where
    go subst (a /\ b) = do
        next <- unify (substitute subst a) (substitute subst b)
        pure $ subst `mergeSubstitutions` next
    | otherwise = throw (unsafePartial $ fromJust span) $ differentLenghts left right
    where
    span | lengthLeft > lengthRight = foldSpan lengthRight left
         | otherwise = foldSpan lengthLeft right
    
    foldSpan smaller = foldlWithIndex (\index maybeSpan term -> if index >= smaller then sourceSpan term # maybe identity (<>) maybeSpan # Just else maybeSpan) Nothing

    lengthLeft = length left
    lengthRight = length right

---------- Variant boilerplate
cannotUnify :: forall r. Expression -> Expression -> Variant ( cannotUnify :: Expression /\ Expression | r )
cannotUnify = curry $ inj (Proxy :: Proxy "cannotUnify")

infiniteVar :: forall r. InfiniteVarError -> Variant ( infiniteVar :: InfiniteVarError | r)
infiniteVar = inj (Proxy :: Proxy "infiniteVar")

differentPattern :: forall r. String -> String -> Variant ( differentPattern :: String /\ String | r)
differentPattern = curry $ inj (Proxy :: Proxy "differentPattern")        

differentLenghts :: forall r. Array Expression -> Array Expression -> Variant ( differentLenghts :: Array Expression /\ Array Expression | r)
differentLenghts = curry $ inj (Proxy :: Proxy "differentLenghts")