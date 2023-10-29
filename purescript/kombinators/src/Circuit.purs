module Kombinator.Circuit where

import Prelude

import Data.Array as A
import Data.Foldable (sum)
import Data.HashMap (HashMap)
import Data.Lens (Lens', _1, _2)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Kombinator.Network (RuntimeWireColor(..), UncoloredNetworkId(..))
import Prim.Boolean (False, True)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---------- Constants
-- | A network channel can be either green or red
data NetworkChannel

-- | The green wire color
foreign import data Green :: NetworkChannel

-- | The red wire color
foreign import data Red :: NetworkChannel

---------- Types
-- TODO: add all of them
data DeciderOperation
  = GreaterThan
  | SmallerThan
  | Equal

data ArithemticOperation
  = Add
  | Multiply
  | Divide
  | Modulo
  | Substract

data Pin
  = IntegerInput Int
  | SignalPin String
  | Anything
  | Everything
  | Each

data ComputationComponent
  = Decider DeciderOperation Pin Pin Pin Boolean
  | Arithemtic ArithemticOperation Pin Pin Pin

type CompleteNetwork = NetworkId Red /\ NetworkId Green

data ComputationPort
  = Both (NetworkId Red) (NetworkId Green)
  | Single UncoloredNetworkId
  | None

data Component
  = ComputationComponent ComputationPort ComputationPort ComputationComponent
  | Constant ComputationPort (HashMap String Int)

newtype NetworkId :: NetworkChannel -> Type
newtype NetworkId a = NetworkId Int

data Circuit
  =
    -- | Red /\ Green wires 
    Network (CompleteNetwork -> Circuit)
  -- | Logical block to split the circuit at. Can have at most 4 external networks
  | Block String Circuit
  | Machine Component
  | Many (Array Circuit)

---------- Tokens used for the custom do notation
data FreshNetwork =
  FreshNetwork

data FreshWire
  = FreshRedWire
  | FreshGreenWire

---------- Helpers
appendComponent :: Circuit -> Component -> Circuit
appendComponent (Many circuits) component = Many (A.snoc circuits (Machine component))
appendComponent other component = Many [ other, Machine component ]

-- | Count the number of combinators inside a circuit
componentCount :: Circuit -> Int
componentCount (Machine _) = 1
componentCount (Many subCircuits) = sum $ componentCount <$> subCircuits
componentCount (Block _ block) = componentCount block
componentCount (Network continue) = componentCount $ continue placeholderNetwork
  where
  placeholderNetwork :: CompleteNetwork
  placeholderNetwork = NetworkId (-1) /\ NetworkId (-1)

-- | Erase the typelevel data about the color of a wire,
-- | instead opting to keep track of it at runtime
forgetTypelevelColorData :: forall wire. IsWire wire => NetworkId wire -> UncoloredNetworkId
forgetTypelevelColorData (NetworkId id) = UncoloredNetworkId (id /\ runtimeColor (Proxy :: _ wire))

---------- Typeclass based syntax-sugar
class CircuitBind f a (d :: Boolean) | a -> f d where
  circuitBind :: a -> f -> Circuit

instance CircuitBind (Unit -> Circuit) Component True where
  circuitBind a f = appendComponent (f unit) a

instance CircuitBind (Unit -> Circuit) Circuit True where
  circuitBind a f = Many [ f unit, a ]

instance CircuitBind (NetworkId Red /\ NetworkId Green -> Circuit) FreshNetwork False where
  circuitBind = const Network

class IsComputationPort f where
  port :: f -> ComputationPort

instance IsComputationPort Unit where
  port _ = None

instance IsComputationPort ComputationPort where
  port = identity

instance IsWire a => IsComputationPort (NetworkId a) where
  port = forgetTypelevelColorData >>> Single

instance IsComputationPort (NetworkId Red /\ NetworkId Green) where
  port = uncurry Both

computation
  :: forall a b
   . IsComputationPort a
  => IsComputationPort b
  => a
  -> b
  -> ComputationComponent
  -> Component
computation a b inner = ComputationComponent (port a) (port b) inner

circuitDiscard :: forall f a. CircuitBind f a True => a -> f -> Circuit
circuitDiscard = circuitBind

emptyCircuit :: Circuit
emptyCircuit = Many []

endCircuit :: Circuit
endCircuit = emptyCircuit

---------- Type-level machinery for wires
-- | Type-class for calculating the inverse of a wire color at the type level
class MirrorableWire :: NetworkChannel -> NetworkChannel -> Constraint
class MirrorableWire wire otherWire | wire -> otherWire, otherWire -> wire

instance MirrorableWire Red Green
instance MirrorableWire Green Red

-- | -- | Class implementing operations which depend on the typelevel color of a network channel 
class IsWire :: NetworkChannel -> Constraint
class IsWire wire where
  -- | Lookup a pair of red /\ green wires by a typelevel channel name
  lookupNetworkChannels :: Proxy wire -> CompleteNetwork -> NetworkId wire

  -- | Cast a typelevel wire color to runtime
  runtimeColor :: Proxy wire -> RuntimeWireColor

instance IsWire Red where
  lookupNetworkChannels _ = fst
  runtimeColor _ = Red

instance IsWire Green where
  lookupNetworkChannels _ = snd
  runtimeColor _ = Green

-- | Index a tuple by assuming red = 0 and green = 1
_atWire :: forall a. RuntimeWireColor -> Lens' (a /\ a) a
_atWire Red = _1
_atWire Green = _2

-- | Mirror a network channel
otherChannel :: forall channel other. MirrorableWire channel other => Proxy channel -> Proxy other
otherChannel = unsafeCoerce

-- | Lookup the network channel not matching a particular id
lookupOtherNetworkChannels
  :: forall wire otherWire
   . MirrorableWire wire otherWire
  => IsWire otherWire
  => Proxy wire
  -> CompleteNetwork
  -> NetworkId otherWire
lookupOtherNetworkChannels = otherChannel >>> lookupNetworkChannels

---------- Operators
-- | Operations which still hasn't gotten it's output
type NoOutputComputation = Pin -> ComputationComponent

-- | Provide an output for an operation
outputTo :: NoOutputComputation -> NoOutputComputation
outputTo f a = f a

computationAdd :: Pin -> Pin -> NoOutputComputation
computationAdd = Arithemtic Add

computationMultiply :: Pin -> Pin -> NoOutputComputation
computationMultiply = Arithemtic Multiply

infix 1 outputTo as /=>
infix 2 computationAdd as /+
infix 2 computationMultiply as /*