module Fix where

import Prelude
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Lazy (defer)
import Data.Lazy as Lazy
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Prim.Row as Row
import Run (FProxy, Run, SProxy(..), VariantF)
import Run as Run
import Undefined (undefined)

data BackwardsState s a
  = BackwardsState (s -> s) (s -> a)

derive instance functorBS ∷ Functor (BackwardsState s)

type BACKWARDS_STATE s
  = FProxy (BackwardsState s)

_backwardsState ∷ SProxy "backwardsState"
_backwardsState = SProxy

liftBackwardsState ∷ forall s a r. BackwardsState s a → Run ( backwardsState ∷ BACKWARDS_STATE s | r ) a
liftBackwardsState = liftBackwardsStateAt _backwardsState

liftBackwardsStateAt ∷
  forall t state a r label.
  IsSymbol label ⇒
  Row.Cons label (BACKWARDS_STATE state) t r ⇒
  SProxy label →
  BackwardsState state a →
  Run r a
liftBackwardsStateAt = Run.lift

runBackwardsStateAt ::
  forall label state rowWithState row a.
  IsSymbol label ⇒
  Row.Cons label (BACKWARDS_STATE state) row rowWithState ⇒
  SProxy label →
  state →
  Run rowWithState a →
  Run row (Tuple state a)
runBackwardsStateAt sym = undefined
  where
  handle ::
    forall b.
    VariantF rowWithState
      (Run rowWithState b) ->
    Either (BackwardsState state (Run rowWithState b)) (VariantF row (Run rowWithState b))
  handle = Run.on sym Left Right

  mkLoop ::
    forall result acc b.
    ( acc ->
      (BackwardsState state (Run rowWithState b)) ->
      Run row result
    ) ->
    (acc -> b -> result) -> acc -> Run rowWithState b -> Run row result
  mkLoop handleState handleResult accumulated r = case Run.peel r of
    Left peeled → case handle peeled of
      Left bs → handleState accumulated bs
      Right a' → Run.send a' >>= mkLoop handleState handleResult accumulated
    Right result → pure $ handleResult accumulated result

  loopState :: Lazy.Lazy state -> Run rowWithState Unit -> Run row (Lazy.Lazy state)
  loopState = mkLoop handleState handleResult
    where
    {-
        past = ...
        present <- mkState future
        future <- mkState past
    -}
    handleState :: state -> BackwardsState state (Run rowWithState Unit) -> Run row (Lazy.Lazy state)
    handleState state (BackwardsState mkState mkValue) =
      sequence do
        (futureState :: Run _ _) <-
          fix \m -> do
            (futureState :: Run _ _) <- m
            let
              value :: Run _ _
              value = mkValue <$> futureState

              state' :: Run _ (Lazy.Lazy state)
              state' = value >>= loopState state
            pure
            state'
        pure (mkState <$> futureState)

    handleResult = const

  --     case Run.peel r of
  --   Left a → case handle a of
  --     Left (BackwardsState mkState _) → do
  --       resultState <- loopState state
  --       pure (mkState futureState)
  --     Right a' → b >>= f
  --       where
  --       f = runBackwardsStateAt sym s
  --       b = Run.send a'
  --   Right a → pure (Tuple s a)
  --   loop :: state -> Run rowWithState a -> Run row (Tuple state a)
  --   loop s r = case Run.peel r of
  --     Left a → case handle a of
  --       Left (BackwardsState mkState mkValue) → do
  --         let
  --           value = mkValue resultState
  --         (Tuple resultState resultValue) <- loop s value
  --         pure $ Tuple (mkState futureState) resultValue
  --       Right a' → b >>= f
  --         where
  --         f :: Run rowWithState a -> Run row (Tuple state a)
  --         f = runBackwardsStateAt sym s
  --         b :: Run row (Run rowWithState a)
  --         b = Run.send a'
  --     Right a → pure (Tuple s a)
  a = 3
