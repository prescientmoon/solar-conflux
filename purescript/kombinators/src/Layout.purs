-- | Generate blueprint layouts from the circuit dsl
module Kombinator.Layout where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2)
import Kombinator.Circuit (Circuit, CompleteNetwork, ComputationPort, forgetTypelevelColorData)
import Kombinator.Circuit as Circuit
import Kombinator.Network (RawNetworkId, UncoloredNetworkId(..))
import Kombinator.PhysicalCircuit (PCMachine, PhysicalCircuit, Vec2, insertWire)
import Kombinator.PhysicalCircuit as PC
import Kombinator.Vector as Vec2
import Run (Run)
import Run.State (STATE, get, modify)
import Run.Supply (SUPPLY, generate)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Constants
combinatorsPerBlock :: Int
combinatorsPerBlock = 46

---------- Effect types
-- | State used while generating individual blocks
type BlockState =
  { circuit :: PhysicalCircuit
  , lastWireOccurences :: HashMap RawNetworkId Vec2
  }

type BlockM r = Run (STATE BlockState + SUPPLY CompleteNetwork + r)

---------- Effect helpers
-- | Insert an entity at the end of the current physical circuit
insertEntity :: forall r. PC.PCEntity -> BlockM r Unit
insertEntity e = modify $ over _circuit $ PC.insertEntity e

-- | Push a new machine onto a physical circuit.
-- | Does not ensure the circuit actually fits in the current block
pushMachine :: forall r. PCMachine -> BlockM r Vec2
pushMachine machine = do
  state <- get
  let machines = state.circuit.entities
  let
    nextPosition = case Array.last machines of
      Nothing -> Vec2.origin
      Just { position }
        | Vec2.x position < 8 -> position # Vec2.mapAxis Vec2.X \p -> p / 2 * 2 + 2
        | otherwise -> vec2 0 (Vec2.y position + 1)
  insertEntity
    { position: nextPosition
    , machine
    }
  pure nextPosition

-- | Check the last position a network was used at.
lastNetworkOccurence :: forall r. RawNetworkId -> BlockM r (Maybe Vec2)
lastNetworkOccurence id = get <#> \s ->
  s.lastWireOccurences
    # HM.lookup id

-- | Update the last occurence of a network
markNetworkUsage :: forall r. RawNetworkId -> Vec2 -> BlockM r Unit
markNetworkUsage id position = modify $ over _lastWireOccurences
  $ HM.insert id position

---------- Implementation
-- | Generate the necessary wiring for a single port
handleRawNetworkId :: forall r. Vec2 -> UncoloredNetworkId -> BlockM r Unit
handleRawNetworkId position (UncoloredNetworkId (id /\ color)) = do
  last <- lastNetworkOccurence id
  case last of
    Nothing -> pure unit
    Just previous -> do
      modify $ over _circuit $
        insertWire color (previous /\ position)
  markNetworkUsage id position

-- | Generate the necessary wiring for a computation port
handleComputationPort :: forall r. Vec2 -> ComputationPort -> BlockM r Unit
handleComputationPort position Circuit.None = pure unit
handleComputationPort position (Circuit.Single id) = handleRawNetworkId position id
handleComputationPort position (Circuit.Both red green) = do
  handleRawNetworkId position $ forgetTypelevelColorData red
  handleRawNetworkId position $ forgetTypelevelColorData green

-- | Generate an individual block of combinators.
-- | Does not check the block does not overflow
generateInBlockIndices :: forall r. Circuit -> BlockM r Unit
generateInBlockIndices (Circuit.Machine (Circuit.Constant port signals)) = do
  -- | Add combinator and wire it to the things around it
  position <- pushMachine $ PC.Constant signals
  handleComputationPort position port

  -- | Light is placed 1 tile over to the right
  let lightPosition = Vec2.mapAxis Vec2.X (_ + 1) position

  -- | Add light to combinator
  insertEntity
    { position: lightPosition
    , machine: PC.Light
        Circuit.GreaterThan
        Circuit.Anything
        (Circuit.IntegerInput 0)
        PC.defaultLightSettings
    }

  -- | Wire light to surroundings
  handleComputationPort lightPosition port
generateInBlockIndices (Circuit.Machine (Circuit.ComputationComponent input output operation)) = do
  position <- pushMachine component
  handleComputationPort position input
  handleComputationPort position input
  where
  component = case operation of
    Circuit.Arithemtic operation p1 p2 p3 -> PC.Arithemtic operation p1 p2 p3
    Circuit.Decider operation p1 p2 p3 output -> PC.Decider operation p1 p2 p3 output
generateInBlockIndices (Circuit.Many circuits) = for_ circuits generateInBlockIndices
generateInBlockIndices (Circuit.Network continue) = do
  network <- generate
  generateInBlockIndices $ continue network
generateInBlockIndices (Circuit.Block name circuit) = do
  -- | TODO: do something witht he name
  generateInBlockIndices circuit

---------- Lenses
_circuit :: Lens' BlockState PhysicalCircuit
_circuit = prop (Proxy :: _ "circuit")

_lastWireOccurences :: Lens' BlockState (HashMap RawNetworkId Vec2)
_lastWireOccurences = prop (Proxy :: _ "lastWireOccurences")