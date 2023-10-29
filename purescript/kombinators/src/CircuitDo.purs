module Kombinator.CircuitDo where

import Prelude (otherwise, show, ($), (-), (<=), (<>), (==))
import Data.Tuple.Nested ((/\))
import Kombinator.Circuit
import Prim.Boolean (True)

bind :: forall a d f. CircuitBind f a d => a -> f -> Circuit
bind = circuitBind

discard :: forall f a. CircuitBind f a True => a -> f -> Circuit
discard = circuitDiscard

---------- Examples
counter :: forall a. IsWire a => String -> NetworkId a -> Circuit
counter signal output = Block "Counter" do
  computation output output (SignalPin signal /+ IntegerInput 1 /=> SignalPin signal)
  endCircuit

buffer :: forall a b. IsComputationPort a => IsComputationPort b => a -> b -> Circuit
buffer from to = do
  computation from to (Each /+ IntegerInput 0 /=> Each)
  endCircuit

-- | Delay a signal by n ticks
delay :: forall a b. IsWire b => IsComputationPort a => Int -> a -> NetworkId b -> Circuit
delay amount input output = Block ("Delay " <> show amount) $ go amount input
  where
  go :: forall i. IsComputationPort i => Int -> i -> Circuit
  go amount input
    | amount <= 0 = endCircuit
    | amount == 1 = do
        buffer input output
        endCircuit
    | otherwise = do
        wire /\ _ <- FreshNetwork
        buffer input wire
        go (amount - 1) wire
        endCircuit

