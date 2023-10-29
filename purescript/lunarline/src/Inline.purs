module Lunarline.Inline where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as A
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Lens (Lens', over, second)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZA
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Lunarline.Ast (Case(..), Expression(..), avoidInfiniteRecursion, occurences, unrollConstructor)
import Run (Run)
import Run as Run
import Run.Except (FAIL, fail, runFail)
import Run.Reader (READER, asks, local, runReader)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Types
type Scope = HashMap String Expression
type Context
  =
  { scope :: Scope
  }

---------- Monad
-- | The base context most inlining operations run inside
type InlineM r = Run (READER Context r)

-- | Same as InlineM, but might fail
type BreakableInlineM r = InlineM (FAIL + r)

-- | The default context containing no bindings in scope
emptyContext :: Context
emptyContext = { scope: HM.empty }

-- | Run a computation inside the InlineM monad
runInlineM :: forall a. Context -> InlineM () a -> a
runInlineM ctx = runReader ctx >>> Run.extract

---------- Helpers
attempt :: forall r a. (a -> Run (FAIL r) a) -> a -> Run r a
attempt f a = runFail (f a) <#> fromMaybe a

lookupScope :: forall r. String -> InlineM r (Maybe Expression)
lookupScope name = asks (_.scope >>> HM.lookup name)

incorporateFailure :: forall r a. Run (FAIL r) (Maybe a) -> Run (FAIL r) a
incorporateFailure = bindFlipped case _ of
  Just success -> pure success
  Nothing -> fail

appendScope :: forall r. String -> Expression -> InlineM r ~> InlineM r
appendScope name expression = local
  (over _scope $ HM.insert name expression)

dropFromScope :: forall r. String -> InlineM r ~> InlineM r
dropFromScope name = local
  (over _scope $ HM.delete name)

extendScopeWith :: forall r. Scope -> InlineM r ~> InlineM r
extendScopeWith extension = local (over _scope $ HM.union extension)

---------- Implementation
executeSafely :: forall r. Expression -> InlineM r ((InlineM r Expression -> InlineM r Expression) /\ Expression)
executeSafely expr = ado
  expr <- execute expr
  in
    go expr
  where
  go = case _ of
    IgnoreVar name body -> (dropFromScope name >>> map (avoidInfiniteRecursion name) >>> restrain) /\ inner
      where
      restrain /\ inner = go body
    other -> identity /\ other

execute :: forall r. Expression -> InlineM r Expression
execute = attempt $ go case _ of
  Var name -> do
    map (avoidInfiniteRecursion name) $ incorporateFailure $ lookupScope name
  IgnoreVar name body -> do
    dropFromScope name $ execute body
  Call function argument -> do
    restrain /\ function <- executeSafely function
    restrain' /\ argument <- executeSafely argument
    restrain $ restrain' case function of
      Lambda name body -> do
        appendScope name argument $ execute body
      _ -> pure $ Call function argument
  Lambda argument body -> do
    restrain /\ body <- dropFromScope argument $ executeSafely body
    restrain $ pure $ Lambda argument body
  Match child cases -> do
    restrain /\ child <- executeSafely child
    restrain case child of
      Match expr innerCases -> execute
        $ Match expr
        $ second (\c -> Match c cases) <$> innerCases
      other -> do
        executeMatch other (ZA.fromNonEmptyArray cases)
  Let name definition continuation -> do
    definition <- execute definition
    continuation <- appendScope name definition $ execute continuation
    pure
      if occurences name definition == 0 && occurences name continuation == 0 then
        continuation
      else Let name definition continuation
  Constructor name -> pure $ Constructor name
  where
  go f expr = do
    res <- f expr
    let a = unsafePerformEffect $ log $ "Inlinng: " <> show expr <> "\nwith: " <> show res <> "\n\n\n"
    pure res

-- | Attempt to create a scope from a pattern match of some case
-- | over some expression
matchCase
  :: Expression -> Case -> Maybe Scope
matchCase expression (Named name) = Just (HM.singleton name expression)
matchCase expression (Deconstruct expectedConstructor childCases)
  | Just (name /\ arguments) <- unrollConstructor expression
  , name == expectedConstructor
  , A.length childCases == A.length arguments =
      ado
        subMatchResults <- for subMatches (uncurry matchCase)
        in A.foldr HM.union HM.empty subMatchResults
      where
      subMatches = A.zip arguments childCases
matchCase _ _ = Nothing

-- | Execute a pattern match expression
executeMatch
  :: forall r
   . Expression
  -> ZipperArray (Case /\ Expression)
  -> BreakableInlineM r Expression
executeMatch child cases = case matchCase child currentCase of
  Nothing -> do
    case ZA.goNext cases of
      Nothing -> ado
        cases <- for (ZA.toNonEmptyArray cases) \(currentCase /\ continuation) -> do
          -- TODO: when encountering 
          --     case expr of A a1 ... an -> body
          -- replace expr with `A a1 ... an` inside the body
          execute continuation <#> (/\) currentCase
        in Match child cases
      Just remaining -> executeMatch child remaining
  Just scopeExtension ->
    extendScopeWith scopeExtension $ execute currentContinuation
  where
  currentCase /\ currentContinuation = ZA.current cases

---------- Lenses
_scope :: Lens' Context Scope
_scope = prop (Proxy :: _ "scope")