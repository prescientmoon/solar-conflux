module Kombinator.PhysicalCircuit where

import Prelude

import Data.Array as Array
import Data.HashMap (HashMap)
import Data.Hashable (class Hashable, hash)
import Data.Lens (Lens', over, set)
import Data.Lens.Record (prop)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec)
import Data.Vec as Vec
import Kombinator.Circuit (_atWire)
import Kombinator.Circuit as C
import Kombinator.Graph.Undirected (Graph, _atGraphConnection)
import Kombinator.Network (RuntimeWireColor)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

---------- Types
type Vec2 = Vec D2 Int
type Pair a = a /\ a
newtype HashableVec2 = HashableVec2 Vec2

-- | All the different kind of poles we can use in factorio
data PoleKind = Small | Medium | Large | Substation

-- | Settings lights can take in factorio
type LightSettings = { colors :: Boolean }

-- | Individual machines which can appear inside blueprints
data PCMachine
  = Constant (HashMap String Int)
  | Arithemtic C.ArithemticOperation C.Pin C.Pin C.Pin
  | Decider C.DeciderOperation C.Pin C.Pin C.Pin Boolean
  | Light C.DeciderOperation C.Pin C.Pin LightSettings
  | Pole PoleKind

type PCEntity =
  { position :: Vec2
  , machine :: PCMachine
  }

type PhysicalCircuit =
  { entities :: Array PCEntity
  , wires :: Pair (Graph HashableVec2)
  }

---------- Constants
defaultLightSettings :: LightSettings
defaultLightSettings = { colors: false }

---------- Helpers
-- | Insert a wire into a physical circuit
insertWire :: RuntimeWireColor -> Pair Vec2 -> PhysicalCircuit -> PhysicalCircuit
insertWire color points = set (_wires <<< _atWire color <<< _atGraphConnection (coerce points)) true

-- | Insert an entity at the end of the entity list
insertEntity :: PCEntity -> PhysicalCircuit -> PhysicalCircuit
insertEntity e = over _entities $ flip Array.snoc e

---------- Lenses
_wires :: Lens' PhysicalCircuit (Pair (Graph HashableVec2))
_wires = prop (Proxy :: _ "wires")

_entities :: Lens' PhysicalCircuit (Array PCEntity)
_entities = prop (Proxy :: _ "entities")

---------- Typeclass isntances
derive instance Eq HashableVec2
instance Hashable HashableVec2 where
  hash (HashableVec2 vec) = hash (vec `Vec.index` d0 /\ vec `Vec.index` d1)