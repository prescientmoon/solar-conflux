module Swictheroo.Stream where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Plus, (<|>))
import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad, iap, iapply, ibind, imap, ipure)
import Control.Parallel (parOneOf, parSequence_)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, never)

-- | A producer can:
-- | - Produce values
-- | - Destroy itself
newtype Producer m a =
  Producer
    { destroy :: m Unit
    -- | Producers advance to a new version once they produce a value.
    -- | This allows pure producers to exist. For example, a pure producer
    -- | could hold an array of events, return the first one, and then create
    -- | a new producer from the tail of the array
    , produce :: m (Lazy (a /\ Producer m a))
    }

produce :: forall m i. Monad m => Producer m i -> m (i /\ Producer m i)
produce (Producer producer) = producer.produce <#> Lazy.force

destroyProducer :: forall m i. Monad m => Producer m i -> m Unit
destroyProducer (Producer producer) = producer.destroy

constantProducer :: forall m a. Monad m => a -> Producer m a
constantProducer value = Producer
  { destroy: pure unit
  , produce: pure $ Lazy.defer \_ -> value /\ (constantProducer value)
  }

unitProducer :: forall m. Monad m => Producer m Unit
unitProducer = constantProducer unit

filterMapProducer :: forall a b m. Monad m => (a -> Maybe b) -> Producer m a -> Producer m b
filterMapProducer f producer = Producer
  { destroy: destroyProducer producer
  , produce: loop producer
  }
  where
  -- Keeps producing values until one matches the given predicate!
  loop producer = do
    value /\ producer' <- produce producer
    case f value of
      Nothing -> loop producer'
      Just updated -> pure $ Lazy.defer \_ -> updated /\ (filterMapProducer f producer')

-- | Type parameter explanation:
-- | - m = underlying monad
-- | - i = what the producer present when 
-- |   the computation *starts* needs to produce
-- | - o = what the producer present when 
-- |   the computation *ends* needs to produce
-- | - a = result of the computation
-- |
-- | This monad encapsulates the followin 3 operations:
-- | - Consuming values from a producer
-- | - Cancelling + replacing the current producer with a different one.
newtype ConsumeM m i o a = ConsumeM
  ( Producer m i -> m (a /\ Producer m o)
  )

pull :: forall m i. Monad m => ConsumeM m i i i
pull = ConsumeM produce

replace :: forall m i o. Monad m => Producer m o -> ConsumeM m i o Unit
replace producer' = ConsumeM \producer -> do
  destroyProducer producer
  pure (unit /\ producer')

lift :: forall m i a. Monad m => m a -> ConsumeM m i i a
lift computation = ConsumeM \producer -> do
  result <- computation
  pure (result /\ producer)

producer :: forall m i. Monad m => ConsumeM m i i (Producer m i)
producer = ConsumeM \p -> pure (p /\ p)

mapSource :: forall m i o. Monad m => (Producer m i -> Producer m o) -> ConsumeM m i o Unit
mapSource f = ConsumeM \producer -> do
  pure (unit /\ f producer)

runConsumeM :: forall m i o a. Monad m => Producer m i -> ConsumeM m i o a -> m a
runConsumeM producer (ConsumeM run) = do
  result /\ producer' <- run producer
  destroyProducer producer'
  pure result

runConsumeM_ :: forall m a o. Monad m => ConsumeM m Unit o a -> m a
runConsumeM_ = runConsumeM unitProducer

---------- Typeclass instances
instance Functor m => Functor (Producer m) where
  map f (Producer producer) = Producer
    { destroy: producer.destroy
    , produce: producer.produce <#> map (bimap f (map f))
    }

instance Functor m => IxFunctor (ConsumeM m) where
  imap f (ConsumeM consumer) = ConsumeM
    \producer -> consumer producer <#> first f

instance Functor m => Functor (ConsumeM m i i) where
  map = imap

instance Monad m => IxApply (ConsumeM m) where
  iapply = iap

instance Monad m => Apply (ConsumeM m i i) where
  apply = iapply

instance Monad m => IxApplicative (ConsumeM m) where
  ipure a = ConsumeM \p -> pure (a /\ p)

instance Monad m => Applicative (ConsumeM m i i) where
  pure = ipure

instance Monad m => IxBind (ConsumeM m) where
  ibind (ConsumeM consumer) f = ConsumeM
    \producer -> do
      result /\ producer' <- consumer producer
      let (ConsumeM consumer') = f result
      consumer' producer'

instance Monad m => Bind (ConsumeM m i i) where
  bind = ibind

instance Monad m => IxMonad (ConsumeM m)
instance Monad m => Monad (ConsumeM m i i)

---------- Merge producers
type AffProducer = Producer Aff

instance Alt AffProducer where
  alt first second = Producer
    { destroy:
        parSequence_
          [ destroyProducer first
          , destroyProducer second
          ]
    , produce: ado
        result <- parOneOf
          [ produce first <#> Left
          , produce second <#> Right
          ]
        in
          pure case result of
            Left (result /\ first') -> result /\ (first' <|> second)
            Right (result /\ second') -> result /\ (first <|> second')
    }

instance Plus AffProducer where
  empty = Producer { destroy: pure unit, produce: never }

