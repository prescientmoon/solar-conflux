module Swictheroo.Stream where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Plus, (<|>))
import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad, iap)
import Control.Parallel (parOneOf, parSequence_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, never)

-- | A producer can:
-- | - Produce values
-- | - Destroy itself
-- |
-- | A producer does this by reacting to input events
-- | in order to satisfy continuations.
newtype Producer m a =
  Producer (forall c. Monad m => ProducerEvent m c a -> m c)

data ProducerEvent m c a
  -- | Producers advance to a new version once they produce a value.
  -- | This allows pure producers to exist. For example, a pure producer
  -- | could hold an array of events, return the first one, and then create
  -- | a new producer from the tail of the array
  = Produce (a -> Producer m a -> c)
  | Destroy c

produce :: forall m i. Monad m => Producer m i -> m (i /\ Producer m i)
produce (Producer producer) = producer (Produce (/\))

destroyProducer :: forall m i. Monad m => Producer m i -> m Unit
destroyProducer (Producer producer) = producer (Destroy unit)

constantProducer :: forall m a. Monad m => a -> Producer m a 
constantProducer value = Producer case _ of
  Destroy c -> pure c
  Produce next -> pure (next value (constantProducer value))

unitProducer :: forall m. Monad m => Producer m Unit 
unitProducer = constantProducer unit

filterMapProducer :: forall a b m. Monad m => (a -> Maybe b) -> Producer m a -> Producer m b
filterMapProducer f producer = Producer case _ of
  Destroy c -> destroyProducer producer $> c
  Produce next -> loop producer
     where
     -- Keeps producing values until one matches the given predicate!
     loop producer = do
      value /\ producer' <- produce producer
      case f value of
        Nothing -> loop producer'
        Just updated -> pure (next updated  (filterMapProducer f producer'))


-- | Type parameter explanation:
-- | - m = underlying monad
-- | - t = type we want to end on
-- | - i = what the producer present when 
-- |   the computation *starts* needs to produce
-- | - o = what the producer present when 
-- |   the computation *ends* needs to produce
-- | - a = result of the computation
-- |
-- | This monad encapsulates the followin 3 operations:
-- | - Basic continuations (we have a type we want to terminate on)
-- | - Consuming values from a producer
-- | - Cancelling + replacing the current producer with a different one.
newtype ConsumeM m t i o a = ConsumeM
  ( forall c
     . { producer :: Producer m i
       , continue :: Producer m o -> a -> m c
       , terminate :: t -> m c
       }
    -> m c
  )

-- | A ConsumeM computation that has finshed running
type Finished m t i = ConsumeM m t i Void Void

pull :: forall m t i. Monad m => ConsumeM m t i i i
pull = ConsumeM \inputs -> do
  i /\ producer' <- produce inputs.producer
  inputs.continue producer' i

replace :: forall m t i o. Monad m => Producer m o -> ConsumeM m t i o Unit
replace producer' = ConsumeM \inputs -> do
  destroyProducer inputs.producer
  inputs.continue producer' unit

terminate :: forall m t i. Monad m => t -> Finished m t i
terminate result = ConsumeM \inputs -> do
  destroyProducer inputs.producer
  inputs.terminate result

lift :: forall m t i a. Monad m => m a -> ConsumeM m t i i a
lift computation = ConsumeM \inputs -> do
  result <- computation
  inputs.continue inputs.producer result

producer :: forall m t i. Monad m => ConsumeM m t i i (Producer m i)
producer = ConsumeM \inputs -> inputs.continue inputs.producer inputs.producer

mapSource :: forall m t i o. Monad m => (Producer m i -> Producer m o) -> ConsumeM m t i o Unit
mapSource f = ConsumeM \inputs -> do 
  let new = f inputs.producer
  inputs.continue new unit

runConsumeM :: forall m i t. Monad m => Producer m i -> Finished m t i -> m t
runConsumeM producer (ConsumeM run) = run
  { producer
  , terminate: pure
  , continue: \producer value -> absurd value -- not eta-reduced for clarity
  }

runConsumeM_ :: forall m t. Monad m => Finished m t Unit -> m t
runConsumeM_ = runConsumeM unitProducer

---------- Typeclass instances
instance Monad m => Functor (Producer m) where
  map f (Producer producer) = Producer case _ of
    Destroy c -> producer (Destroy c)
    Produce next -> producer $ Produce \a p -> next (f a) (map f p)

instance Monad m => IxFunctor (ConsumeM m t) where
  imap f (ConsumeM consumer) = ConsumeM
    \inputs -> consumer
      { producer: inputs.producer
      , terminate: inputs.terminate
      , continue: \producer a -> inputs.continue producer (f a)
      }

instance Monad m => IxApply (ConsumeM m t) where
  iapply = iap

instance Monad m => IxApplicative (ConsumeM m t) where
  ipure a = ConsumeM \inputs -> inputs.continue inputs.producer a

instance Monad m => IxBind (ConsumeM m t) where
  ibind (ConsumeM consumer) f = ConsumeM
    \inputs -> do
      consumerResult <- consumer
        { producer: inputs.producer
        , terminate: \result -> pure (Left result)
        , continue: \a b -> pure (Right (a /\ b))
        }

      case consumerResult of
        -- Terminated
        Left final -> inputs.terminate final

        -- Kept going
        Right (producer' /\ result) -> do
          let (ConsumeM consumer') = f result

          consumer'
            { producer: producer'
            , continue: inputs.continue
            , terminate: inputs.terminate
            }

instance Monad m => IxMonad (ConsumeM m t)

---------- Merge producers
type AffProducer = Producer Aff

instance Alt AffProducer where
  alt first second = Producer case _ of
    Produce continue -> ado
      result <- parOneOf
        [ produce first <#> Left
        , produce second <#> Right
        ]
      in
        case result of
          Left (result /\ first') -> continue result (first' <|> second)
          Right (result /\ second') -> continue result (first <|> second')

    Destroy continue -> ado
      parSequence_
        [ destroyProducer first
        , destroyProducer second
        ]
      in continue

instance Plus AffProducer where
  empty = Producer \_ -> never

