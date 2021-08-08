-- | Alternative representation of State allowing for different interpreters
module Run.State.External where

import Prelude

import Data.Lens (Fold, Getter, Lens', Setter)
import Data.Lens as Lens
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Run (EFFECT, Run)
import Run as Run
import Run.State (STATE)
import Run.State as State
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Types
-- | Alternative representation of State allowing for different interpreters
data ExternalState s a
    = Get (s -> a)
    | Put s a

type EXTERNAL_STATE s r = ( externalState :: ExternalState s | r )

-- | Pair of getter and setter functions for interpreting some external state
type StateRunner m s =
    { get :: m s
    , set :: s -> m Unit
    }

---------- Constructors
-- | Get the current state
get :: forall r s. Run (EXTERNAL_STATE s r) s
get = Run.lift _externalState $ Get identity

-- | Set the current state
put :: forall r s. s -> Run (EXTERNAL_STATE s r) Unit
put s = Run.lift _externalState $ Put s unit

---------- Interpreters
-- | Run some external state by using a pair of get and set functions
runExternalState :: forall s r. StateRunner (Run r) s -> Run (EXTERNAL_STATE s r) ~> Run r
runExternalState { get, set } = Run.interpret (Run.on _externalState handle Run.send)
    where
    handle :: forall a. ExternalState s a -> Run r a
    handle (Get continue) = get <#> continue
    handle (Put state next) = (set state) $> next

-- | Run the external state by calling some effects
runExternalStateEffectfully :: forall s r. StateRunner Effect s -> Run (EXTERNAL_STATE s + EFFECT r) ~> Run (EFFECT r)
runExternalStateEffectfully { get, set } = runExternalState
    { get: liftEffect get
    , set: set >>> liftEffect
    }

-- | Save the external state inside a ref 
runExternalStateUsingRef :: forall s r. Ref s -> Run (EXTERNAL_STATE s + EFFECT r) ~> Run (EFFECT r)
runExternalStateUsingRef ref = runExternalStateEffectfully
    { get: Ref.read ref
    , set: flip Ref.write ref
    }

-- | External state is invariant over the state type parameter, so we can imap is.
imapExternaState :: forall s a r. (s -> a) -> (a -> s) -> Run (EXTERNAL_STATE a + EXTERNAL_STATE s r) ~> Run (EXTERNAL_STATE s r)
imapExternaState from to = runExternalState
    { get: gets from
    , set: to >>> put
    }

-- | Run some state under the focus of a bigger state
focusExternalState :: forall s a r. Lens' s a -> Run (EXTERNAL_STATE a + EXTERNAL_STATE s r) ~> Run (EXTERNAL_STATE s r)
focusExternalState lens = runExternalState
    { get: use lens
    , set: assign lens
    }

-- | Convert the external state to internal state
toInternalState :: forall r s a. Run (EXTERNAL_STATE s + STATE s r) a -> Run (STATE s r) a
toInternalState = runExternalState
    { get: State.get
    , set: State.put
    }

-- | Converts the state to internal state and then runs it purely
runExternalStatePure :: forall r s a. s -> Run (EXTERNAL_STATE s + STATE s r) a -> Run r (s /\ a)
runExternalStatePure initial = toInternalState >>> State.runState initial

---------- Helpers
-- | Get the current state and run a function over it
gets :: forall r a b. (a -> b) -> Run (EXTERNAL_STATE a r) b
gets f = get <#> f

-- | Run a function over the current state
modify :: forall r s. (s -> s) -> Run (EXTERNAL_STATE s r) s
modify f = do
    new <- gets f
    put new $> new

-- | Run a function over the current state, discarding the result
modify_ :: forall r s. (s -> s) -> Run (EXTERNAL_STATE s r) Unit
modify_ f = void $ modify f

---------- Lens helpers
-- | Run a function by focusing on the state using an optic
modifying :: forall s a b r. Setter s s a b -> (a -> b) -> Run (EXTERNAL_STATE s r) Unit
modifying lens f = modify_ (Lens.over lens f)

-- | Set the value of a focus on the state
assign :: forall s a b r. Setter s s a b -> b -> Run (EXTERNAL_STATE s r) Unit
assign lens v = modify_ (Lens.set lens v)

-- | Get the value of a focus on the state
use :: forall s t a b r. Getter s t a b -> Run (EXTERNAL_STATE s r) a
use lens = gets $ Lens.view lens

-- | Attempt getting the value of a focus on the state
preuse :: forall s t a b r. Fold (First a) s t a b -> Run (EXTERNAL_STATE s r) (Maybe a)
preuse lens = gets $ Lens.preview lens

---------- SProxies
_externalState :: Proxy "externalState"
_externalState = Proxy

---------- Typeclass isntances
derive instance Functor (ExternalState s)