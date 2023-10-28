module Canopy.DipMap where

import Canopy.Graph (Graph)
import Data.List (List)
import Data.Maybe (Maybe)

newtype DipUnit c = DipUnit
  { nation :: c
  , isFleet :: Boolean
  }

newtype Territory c = Territory
  { isSea :: Boolean
  , unit :: Maybe (DipUnit c)
  }

type DipMap c = Graph (Territory c)

-- Index in some DipMap
type Loc = Int

data Move
  = Attack Loc
  | Support Loc Loc
  | Convoy Loc Loc
  | Hold

type DipMoves = List Move
