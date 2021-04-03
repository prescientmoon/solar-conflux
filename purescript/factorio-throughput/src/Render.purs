module Moontorio.Render where

import Prelude

import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import RealFunction (Constraints, Factory, PortSide(..), RealFunction, factoryPorts, tryFindBoundPure, tryFindValuePure)

type RenderFn = String -> Array RealFunction -> Effect Unit

renderFactory :: RenderFn -> Factory -> Constraints -> Effect Unit
renderFactory render factory constraints = for_ (factoryPorts factory) \portId -> do
    let inputMax = tryFindBoundPure (portId /\ Input) constraints
    let outputMax = tryFindBoundPure (portId /\ Output) constraints
    let actual = tryFindValuePure portId constraints

    render ("Port " <> show portId) [inputMax, outputMax, actual]