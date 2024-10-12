module BackwardsState where

import Prelude
import Control.MonadFix (class MonadFix, mfix)
import Data.Lazy (Lazy, force)
import Data.Lazy as Lazy
import Data.Newtype as Newtype
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))

newtype BackwardsState s m a
  = BackwardsState (Lazy s -> m (Tuple a (Lazy s)))

runBackwardsState :: forall s m a. Functor m => BackwardsState s m a -> Lazy s -> m (Tuple a s)
runBackwardsState (BackwardsState run) s = run s <#> map force

put :: forall s m. Monad m => s -> BackwardsState s m Unit
put s = BackwardsState \old -> pure $ Tuple unit (pure s)

putLazy :: forall s m. Monad m => Lazy s -> BackwardsState s m Unit
putLazy s = BackwardsState \old -> pure $ Tuple unit s

get :: forall m s. Monad m => BackwardsState s m (Lazy s)
get = BackwardsState \s -> pure $ Tuple s s

modify :: forall m s. Monad m => (s -> s) -> BackwardsState s m Unit
modify f = BackwardsState \s -> pure $ Tuple unit (f <$> s)

derive instance newtypeBackwardsState :: Newtype.Newtype (BackwardsState s m a) _

instance functorBS ∷ Functor m => Functor (BackwardsState s m) where
  map = first >>> map >>> compose >>> Newtype.over BackwardsState

instance applicativeBackwardsState :: MonadFix m => Applicative (BackwardsState s m) where
  pure a = BackwardsState (\s -> pure $ Tuple a s)

instance applyBackwardsState :: MonadFix m => Apply (BackwardsState s m) where
  apply = ap

instance bindBackwardsState :: MonadFix m => Bind (BackwardsState s m) where
  bind (BackwardsState run) f =
    BackwardsState \state ->
      _.results
        <$> mfix \lazy -> do
            (Tuple presentValue presentState) <-
              run
                $ Lazy.defer \_ -> force $ _.future $ lazy unit
            (Tuple futureValue futureState) <- Newtype.unwrap (f presentValue) state
            pure { results: Tuple futureValue presentState, future: futureState }

instance monadBackwardsState :: MonadFix m => Monad (BackwardsState s m)
