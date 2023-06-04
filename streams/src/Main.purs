module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Pipes (await, for, yield, (>->))
import Pipes.Core (Consumer_, Producer_, runEffect)
import Pipes.Prelude (take)
import Pipes.Prelude as Pipes

naturals :: forall m. Monad m => Producer_ Int m Unit
naturals = go 0
  where
  go n = do
    yield n
    go (n + 1)

logAll :: forall a m. MonadEffect m => Show a => Consumer_ a m Unit
logAll = do
  showable <- await
  logShow showable
  logAll

nNaturals :: forall m. Monad m => Int -> Producer_ Int m Unit
nNaturals n = naturals >-> take n

merger :: forall m. Monad m => Producer_ (Tuple Int Int) m Unit
merger = 
  for (nNaturals 3) \a ->
    for (nNaturals 3 >-> Pipes.map ((+) 4)) \b ->
      yield $ Tuple a b


main :: Effect Unit
main = runEffect ((nNaturals 3 <> nNaturals 3) >-> logAll)
