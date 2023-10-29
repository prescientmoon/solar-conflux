module Lambda where

import Prelude

import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))
import Existsential (Exists, mkExists, runExists)

data CallData t f = CallData f (f -> t)
data CallF a t = CallF (Exists (CallData t)) (t -> a)

data LambdaF a
    = Call (Exists (CallF a))

type Lambda = Free LambdaF

call :: forall a b. (a -> b) -> a -> Lambda b
call f arg = liftF $ Call (mkExists (CallF callData identity))
    where
    callData = mkExists (CallData arg f)

eval :: forall a. Lambda a -> a
eval = resume >>> case _ of
    Left (Call ex) -> eval $ run1 ex
        where 
        run1 :: forall r. Exists (CallF r) -> r
        run1 = runExists (\(CallF ex' cb) -> cb $ run2 ex')

        run2 :: forall t. Exists (CallData t) -> t
        run2 = runExists (\(CallData arg f) -> f arg)
    Right result -> result

test :: Lambda Int
test = do
    result <- call ((+) 2) 3 
    pure (result * 3)

instance functorLambdaF :: Functor LambdaF where
    map f (Call ex) = Call $ flip runExists ex (\(CallF ex' f') -> mkExists (CallF ex' $ f' >>> f))

-- instance showLambda :: Show a => Show (LambdaF a) where
--     show (Call left right) = show left <> " " <> show right
--     show (Lambda arg body) = "(\\" <> arg <> " -> " <> show body <> ")"