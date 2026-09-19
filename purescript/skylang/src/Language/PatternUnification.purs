module Sky.Language.PatternUnification
  ( NameMapping
  , PartialRenaming(..)
  , codomainSize
  , domainSize
  , extendRenaming
  , invert
  , lookupRenaming
  , rename
  , solve
  , unify
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldWithIndexM)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Run (Run)
import Safe.Coerce (coerce)
import Sky.Language.Effects (SkyM, extendPrintScope, generateVar, getDepth, increaseDepth, lookupSource)
import Sky.Language.Error (class SourceSpot, MetaError(..), SKY_ERROR, UnificationError(..), lambdaArgument, piArgument, throwMetaError, throwUnificationError)
import Sky.Language.Eval (applyClosure, evalWith, force, getDepthMarker, quoteIndex, vApply)
import Sky.Language.Log (unifying)
import Sky.Language.MetaVar (solveMeta)
import Sky.Language.Term (Closure, Level(..), MetaVar, NameEnv(..), Spine(..), Term(..), Value(..))

---------- Types
-- | Renmaming between two contexts
type NameMapping = HashMap Level Level

-- | A renaming between 2 contexts.
-- | Partial because lookups are not total.
newtype PartialRenaming =
  PartialRenaming
    { domainSize :: Level
    , -- size of A
      codomainSize :: Level
    , -- size of B
      mapping :: NameMapping -- mapping from vars in A to vars in B
    }

---------- Helpers
-- | Get the size of the domain in a renaming
domainSize :: PartialRenaming -> Level
domainSize (PartialRenaming { domainSize }) = domainSize

-- | Get the size of the codomain in a renaming
codomainSize :: PartialRenaming -> Level
codomainSize (PartialRenaming { codomainSize }) = codomainSize

-- | Lookupg what the coresponding name in the codomain
-- | a given name in the domain has.
-- | Returns a Maybe because the renaming is Partial
lookupRenaming :: Level -> PartialRenaming -> Maybe Level
lookupRenaming key (PartialRenaming { mapping }) = HM.lookup key mapping

-- | Increase the size of the maping by 1.
-- | Example: [0: 0, 3: 1, 5: 2] 
-- |  becomes [0: 0, 3: 1, 5: 2, 6: 3]
extendRenaming :: PartialRenaming -> PartialRenaming
extendRenaming (PartialRenaming { domainSize, codomainSize, mapping }) =
  PartialRenaming
    { domainSize: increase domainSize
    , codomainSize: increase codomainSize
    , mapping: HM.insert domainSize codomainSize mapping
    }
  where
  increase :: Level -> Level
  increase = coerce ((+) 1)

-- | Attempts to produce a renaming by inverting a spine of variables.
-- | Throws errors if the same variable appears multiple times inside the spine,
-- | Or if terms in the spine are not variables
invert :: forall a r. Level -> Spine a -> Run (SKY_ERROR a r) PartialRenaming
invert domainSize (Spine spine) = do
  mapping <- foldWithIndexM invertSingleTerm HM.empty spine
  pure $ PartialRenaming
    { domainSize
    , codomainSize: Level $ Array.length spine
    , mapping
    }
  where
  invertSingleTerm index mapping = case _ of
    VSourceAnnotation _ inner -> invertSingleTerm index mapping inner
    VVariableApplication source var (Spine []) ->
      if HM.member var mapping then
        throwMetaError $ NonLinearPattern { spine: coerce spine, varSource: source }
      else
        pure $ HM.insert var (Level index) mapping
    argument -> throwMetaError $ PatternArgumentIsNotVar
      { spine: coerce spine, argument }

rename
  :: forall a r
   . SourceSpot a
  => MetaVar
  -> PartialRenaming
  -> Value a
  -> SkyM a r (Term a)
rename meta renaming value = go renaming value -- not eta reducing because of error reporting
  where
  goSpine :: a -> PartialRenaming -> Term a -> Spine a -> _ (Term a)
  goSpine source renaming term (Spine spine) =
    Array.foldM (spineFolder source renaming) term spine

  spineFolder :: a -> PartialRenaming -> Term a -> Value a -> _ (Term a)
  spineFolder source renaming term argument = ado
    argument <- go renaming argument
    in Application source term argument

  go :: PartialRenaming -> Value a -> _ (Term a)
  go renaming unforced = do
    forced <- force unforced
    case forced of
      VStar source -> pure $ Star source
      VSourceAnnotation source inner ->
        -- TODO: handle errors coming from underneat
        go renaming inner <#> SourceAnnotation source
      VPi source domain codomain -> do
        domain <- go renaming domain
        -- | TODO: make sure we are using the right size here
        codomain <- applyClosure codomain
          ( VVariableApplication (piArgument source)
              (domainSize renaming)
              mempty
          )
        codomain <- go (extendRenaming renaming) codomain
        pure $ Pi source domain codomain
      VLambda source body -> do
        body <- applyClosure body
          ( VVariableApplication (lambdaArgument source)
              (domainSize renaming)
              mempty
          )
        body <- go (extendRenaming renaming) body
        pure $ Lambda source body
      VMetaApplication source otherMeta spine
        | meta == otherMeta -> throwUnificationError $ FailedOccursCheck
            { meta, occursAt: source, occursIn: value }
        | otherwise ->
            goSpine source renaming (Meta source otherMeta) spine
      VAssumptionApplication source type_ spine -> do
        type_ <- go renaming type_
        goSpine source renaming type_ spine
      VVariableApplication source index spine ->
        case lookupRenaming index renaming of
          Nothing -> throwUnificationError $ EscapingVariable
            { source, whileRenaming: value, variable: index }
          Just newName -> do
            goSpine source renaming
              (Var source (quoteIndex (codomainSize renaming) newName))
              spine

-- | Wrap a term in a given number of lambdas
wrapInLambdas :: forall a. a -> Int -> Term a -> Term a
wrapInLambdas source 0 term = term
wrapInLambdas source n term = Lambda source $ wrapInLambdas source (n - 1) term

-- | Solve a meta unification problem:
-- |     ?a spine = rhs
solve
  :: forall a r
   . SourceSpot a
  => a
  -> Level
  -> MetaVar
  -> Spine a
  -> Value a
  -> SkyM a r Unit
solve source depth meta spine rhs = do
  renaming <- invert depth spine
  rhs <- rename meta renaming rhs
  solution <- evalWith (NameEnv [ "TODOFIXTHIS" ]) mempty $ wrapInLambdas
    source
    (coerce $ codomainSize renaming)
    rhs
  solveMeta meta solution

-- | Helper for unifying 2 closures by creating a marker and applying it to both sides
unifyClosures :: forall a r. SourceSpot a => a -> a -> Closure a -> Closure a -> SkyM a r Unit
unifyClosures sourceL sourceR lhs rhs = do
  markerL <- getDepthMarker sourceL
  markerR <- getDepthMarker sourceR
  lhs <- applyClosure lhs markerL
  rhs <- applyClosure rhs markerR
  var <- generateVar
  extendPrintScope [ var ] $ increaseDepth $ unify lhs rhs

-- | Helper for unifying 2 spines, element by element
unifySpines :: forall a r. SourceSpot a => Spine a -> Spine a -> SkyM a r Unit
unifySpines (Spine lhs) (Spine rhs) = void $ Array.zipWithA unify lhs rhs

-- | Makes sure two values are equal, solving meta variables along the way
unify
  :: forall a r
   . SourceSpot a
  => Value a
  -> Value a
  -> SkyM a r Unit
unify lhs rhs = unifying lhs rhs do
  lhs <- force lhs
  rhs <- force rhs
  unify' lhs rhs

-- | Actual implementation for unification. Expects both sides to have been forced.
unify'
  :: forall a r
   . SourceSpot a
  => Value a
  -> Value a
  -> SkyM a r Unit
unify' = case _, _ of
  VStar _, VStar _ -> pure unit
  VPi sourceL domainL codomainL, VPi sourceR domainR codomainR -> ado
    -- ado notation is pointless here, but I want to preserve the notion 
    -- of using the applicative instance here (means the code could be parallelized)
    unify domainL domainR
    unifyClosures (piArgument sourceL) (piArgument sourceR) codomainL codomainR
    in unit
  VLambda sourceL lhs, VLambda sourceR rhs ->
    unifyClosures (lambdaArgument sourceL) (lambdaArgument sourceR) lhs rhs
  VLambda source body, other -> do
    marker <- getDepthMarker (lambdaArgument source)
    body <- applyClosure body marker
    other <- vApply other marker
    name <- lookupSource (lambdaArgument source)
    extendPrintScope [ name ] $ increaseDepth $ unify body other
  other, lambda@(VLambda _ _) -> unify' lambda other -- just swap positions and reuse the existing code
  VVariableApplication _ varL spineL, VVariableApplication _ varR spineR
    | varL == varR -> unifySpines spineL spineR
  VAssumptionApplication _ lhs spineL, VAssumptionApplication _ rhs spineR -> do
    unify lhs rhs
    unifySpines spineL spineR
  VMetaApplication _ metaL spineL, VMetaApplication _ metaR spineR
    | metaL == metaR -> unifySpines spineL spineR
  VMetaApplication source meta spine, other -> do
    depth <- getDepth
    solve source depth meta spine other
  other, meta@(VMetaApplication _ _ _) -> unify' meta other
  -- We skip forcing, because the implementation for `force`
  -- is smart enough to skip over source annotations already
  VSourceAnnotation _ lhs, rhs -> unify' lhs rhs
  lhs, VSourceAnnotation _ rhs -> unify' lhs rhs
  -- If none of the existing cases match, we just give up and throw an error
  lhs, rhs -> throwUnificationError $ CannotUnify { lhs, rhs }
