-- | Modified version of the code from [record-extra](https://github.com/justinwoo/purescript-record-extra/blob/v4.0.0/src/Record/Extra.purs#L123-L127)
-- | I did not like how that lib used lists, so I modified it to output to an (initially) mutable array
module Thing.Ecs.Utils.Keys (keys, class Keys, keysImpl) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

-- | Typeclass jk
class Keys (xs :: RL.RowList Type) where
  keysImpl :: forall r. Proxy xs -> ST r (STArray r String)

instance Keys RL.Nil where
  keysImpl _ = STArray.empty

instance
  ( IsSymbol name
  , Keys tail
  ) =>
  Keys (RL.Cons name ty tail) where
  keysImpl _ = do
    rest <- keysImpl (Proxy :: _ tail)
    _ <- STArray.push first rest
    pure rest
    where
    first = reflectSymbol (Proxy :: _ name)

-- | Extract a value level array of the keys of a row
keys
  :: forall g row rl
   . RL.RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> Array String
keys _ = STArray.run (keysImpl (Proxy :: _ rl))