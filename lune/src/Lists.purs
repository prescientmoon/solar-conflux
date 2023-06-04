module Lists where


foreign import kind TList
foreign import data TNil :: TList
foreign import data TCons :: Type -> TList -> TList

data TLProxy (tlist :: TList) = TLProxy


foreign import kind RList
foreign import data RNil :: RList
foreign import data RCons :: #Type -> RList -> RList

data RLProxy (tlist :: RList) = RLProxy

infixr 6 type RCons as :

-- | Check if a rlist contains a row
class InList (t :: #Type) (list :: RList)

instance inListFirst :: InList t (t : rest)
else instance inListLater :: InList t tail => InList t (head : tail)

-- | Remove an element from a list
class Without (t :: #Type) (list :: RList) (remaining :: RList) | t list -> remaining

instance withoutNil :: Without a RNil RNil
else instance withoutHead :: Without a (RCons a tail) tail
else instance withoutCons :: Without a tail remaining => Without a (RCons head tail) (RCons head remaining)

-- | Break up function into it's components
class MatchArrow t (arguments :: TList) return 
  | t -> arguments return
  , arguments return -> t
  , arguments t -> return 

instance matchArrowRec 
  :: MatchArrow to arguments return 
  => MatchArrow (from -> to) (TCons from arguments) return

else instance matchArrowGeneral :: MatchArrow t TNil t