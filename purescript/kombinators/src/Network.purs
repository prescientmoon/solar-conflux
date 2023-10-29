-- | General stuff related to wire networks
module Kombinator.Network where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Tuple.Nested (type (/\), (/\))

---------- Types
-- | Runtime representation for the color of a wire
data RuntimeWireColor = Green | Red

-- | Network id which does not hold any typelevel evidence for it's color
newtype UncoloredNetworkId = UncoloredNetworkId (Int /\ RuntimeWireColor)

-- | The raw, no color data, id of a network
type RawNetworkId = Int

---------- Helpers
-- | Extract the id from an uncolored network id
uncoloredToRawId :: UncoloredNetworkId -> RawNetworkId
uncoloredToRawId = hash

---------- Typeclass instances
derive instance Eq RuntimeWireColor
derive instance Eq UncoloredNetworkId

instance Hashable RuntimeWireColor where
  hash Green = 0
  hash Red = 1

instance Hashable UncoloredNetworkId where
  hash (UncoloredNetworkId (id /\ _)) = id
