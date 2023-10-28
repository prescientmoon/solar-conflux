module Handle where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, runFn3, runFn4)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (Foreign, unsafeToForeign)
import Lists (class MatchArrow, TCons, TNil, kind TList)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RowList
import Type.Data.Row (RProxy(..))

-- | Abilities for testing
type ReaderAbility e r = ( ask :: e | r ) 
type StreamAbility v r = ( emit :: v -> Unit | r )
type StoreAbility s r 
  = ( get :: s
    , put :: s -> Unit | r )

-- | Transform a list of types into a chain of nested tuples.
class TListToTuples (i :: TList) o | i -> o

instance tlistTupleConsNil :: TListToTuples (TCons head TNil) head
else instance tlistTupleCons 
  :: TListToTuples tail outputTail 
  => TListToTuples (TCons head tail) (head /\ outputTail)
else instance tlistTupleBase :: TListToTuples TNil Unit

-- | Transform a chain of nested tuples into a list of types
class TuplesToTList i (o :: TList) | i -> o

instance tupleToTListTuple 
  :: TuplesToTList b tail
  => TuplesToTList (a /\ b) (TCons a tail)
else instance tupleToTListUnit :: TuplesToTList Unit TNil
else instance tupleToTListGeneral :: TuplesToTList a (TCons a TNil)

-- | Measure the lenght of some nested tuples
class TupleLength t where
  tupleLength :: Proxy t -> Int

instance tupleLengthTuple :: TupleLength b => TupleLength (a /\ b) where
  tupleLength _ = tupleLength (Proxy :: _ b) + 1

else instance tupleLengthUnit :: TupleLength Unit where
  tupleLength _ = 0

else instance tupleLengthGeneral :: TupleLength a where
  tupleLength _ = 1

{-
get :: s 
put :: s -> ()

...becomes
get :: (s -> Lune abilities a) -> Lune remaining result
put :: (unit -> Lune abilities a) -> state -> Lune remaining result
-}
class AbilityMatchers (all :: #Type) a result (abilities :: #Type) (matchers :: #Type) 
  | all a result abilities -> matchers
  , all a result matchers -> abilities

instance abilityMatchersGeneral :: 
  ( RowList.RowToList abilities abilities'
  , AbilityMatchersRL (Lune all a) result abilities' matchers
  ) => AbilityMatchers all a result abilities ( pure :: a -> result | matchers )

-- | Internal version of abilityMatchers using rowlists
class AbilityMatchersRL next result (rowList :: RowList.RowList) (output :: #Type) 
  | next result rowList -> output
  , next result output -> rowList

instance abilityMatchersRLNil :: AbilityMatchersRL next result RowList.Nil ()
else instance abilityMatchersRlCons :: 
  ( AbilityMatchersRL continuationOutput result tail tail' 
  , MatchArrow focus parameters continuationInput
  , MatchArrow return parameters result
  , Row.Cons key ((continuationInput -> continuationOutput) -> return) tail' matchers
  ) => AbilityMatchersRL
        continuationOutput 
        result 
        (RowList.Cons key focus tail) 
        matchers

-- | Type for lune requests
type Request abilities a = (forall t.
      (forall key focus subrow arguments return tuples. 
        Row.Cons key focus subrow abilities =>
        MatchArrow focus arguments return =>
        TListToTuples arguments tuples => 
        IsSymbol key =>
        SProxy key ->
        Int ->
        tuples -> 
        (return -> Lune abilities a) -> 
        t
      ) -> t)

-- | The actual lune monad
data Lune (abilities :: #Type) a 
  = Pure a
  | Request (Request abilities a)

-- TODO: ffi this
foreign import curryImpl :: 
  forall tuples arguments return t. 
  MatchArrow t arguments return => 
  TListToTuples arguments tuples => 
  Fn3 
    (forall a b. a -> b -> Tuple a b)
    Int
    (tuples -> return) 
    t

curryGeneralized :: 
  forall tuples arguments return t.
  MatchArrow t arguments return => 
  TListToTuples arguments tuples => 
  TupleLength tuples =>
  Proxy t ->
  (tuples -> return) ->
  t
curryGeneralized _ = runFn3 curryImpl (/\) (tupleLength (Proxy :: _ tuples))

request :: 
  forall abilities key focus subrow output arguments return tuples.  
  Row.Cons key focus subrow abilities => 
  MatchArrow focus arguments return => 
  TListToTuples arguments tuples => 
  MatchArrow output arguments (Lune abilities return) =>
  IsSymbol key =>
  RProxy abilities -> 
  SProxy key ->
  output
request r s = curryGeneralized (Proxy :: _ output) $ \t -> Request \f -> f s (tupleLength (Proxy :: _ tuples)) t Pure

type MatchLune abilities a t = 
  Fn2 
    (Lune abilities a) 
    { pure :: a -> t
    -- TODO: this is not that well typed, maybe try and fix it
    , request :: 
        Fn4 Int String Foreign Foreign t
    } t

matchLune :: forall abilities a t. MatchLune abilities a t
matchLune = mkFn2 \lune cases -> case lune of
  Pure a -> cases.pure a
  Request r -> r \sproxy argumentCount t a -> 
    runFn4 cases.request 
      argumentCount 
      (reflectSymbol sproxy) 
      (unsafeToForeign t) 
      (unsafeToForeign a)

foreign import handleImpl :: 
  forall abilities subrow remaining result a matchers.
  Row.Union subrow remaining abilities =>
  AbilityMatchers abilities a (Lune remaining result) subrow matchers =>
  Fn4 
    (MatchLune abilities a (Lune abilities result)) 
    (forall x y. Request x y -> Lune x y) 
    (Record matchers) 
    (Lune abilities a) 
    (Lune remaining result)

-- | Remove some effects.
handleWith :: 
  forall abilities subrow remaining result a matchers.
  Row.Union subrow remaining abilities =>
  AbilityMatchers abilities a (Lune remaining result) subrow matchers =>
  RProxy subrow ->
  Record matchers ->
  Lune abilities a -> 
  Lune remaining result
handleWith _ matchers monad
  = runFn4 handleImpl 
      matchLune 
      Request 
      matchers
      monad


-- | Why is this not a thing already?
data Proxy t = Proxy

-- Tests
getState :: forall a. Lune (StoreAbility a ()) a
getState = request _store _get
  where
  _store :: RProxy (StoreAbility a ())
  _store = RProxy

  _get :: SProxy "get"
  _get = SProxy

putState :: forall a. a -> Lune (StoreAbility a ()) Unit
putState s = request _store _put s
  where
  _store :: RProxy (StoreAbility a ())
  _store = RProxy

  _put :: SProxy "put"
  _put = SProxy
 
-- Typeclass instances for Lune.
instance functorLune :: Functor (Lune abilities) where
  map f = case _ of
    Pure a -> Pure (f a)
    Request existential -> 
      Request \runExistential -> 
        existential \key paramCount parameters continuation -> 
          runExistential key paramCount parameters (continuation >>> map f)

instance applyLune :: Apply (Lune abilities) where
  apply = ap

instance applicativeLune :: Applicative (Lune abilities) where
  pure = Pure

instance bindLune :: Bind (Lune abilities) where
  bind m f = case m of
    Pure a -> f a
    Request existential -> 
      Request \runExistential -> 
        existential \key paramCount parameters continuation ->
          runExistential key paramCount parameters (continuation >=> f)

instance monadLune :: Monad (Lune abilities)

-- | Extract the value from an empty lune monad.
extract :: forall a. Lune () a -> a
extract = case _ of
  Pure a -> a
  -- | This should never run
  Request _ -> unsafeCrashWith "Cannot extract values from requests"

-- | Simple program for testing
myProgram :: Lune (StoreAbility Int ()) String
myProgram = do
  state <- getState
  putState (state + 3)
  pure $ show $ state * 2

-- | Handler for stores
runStore :: forall state a r. state -> Lune (StoreAbility state r) a -> Lune r (state /\ a)
runStore initialState = handleWith _on (handler initialState)
  where
  handler state = 
    { get: \continue -> handleWith _on (handler state) (continue state)
    , put: \continue newState -> handleWith _on (handler newState) (continue unit)
    , pure: \a -> pure (state /\ a)
    }

  _on :: RProxy (StoreAbility state ())
  _on = RProxy

abilityMatchers :: 
  forall all a result abilities matchers. 
  AbilityMatchers all a result abilities matchers =>
  Proxy (Lune all a) ->
  Proxy result ->
  RProxy abilities ->
  RProxy matchers
abilityMatchers _ _ _ = RProxy
