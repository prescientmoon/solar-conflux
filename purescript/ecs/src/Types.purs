module Thing.Ecs.Types
  ( Cache
  , ComponentManager
  , ComponentSet
  , ComponentSpec
  , Ecs
  , EcsSpec
  , EntityId
  , Group
  , GroupSpec
  , MakeComponentSet
  , MemMappedComponent
  , MemPool
  , OpaqueComponentManager
  , SelectGroupComponents
  , createEntity
  , createGroup
  , deleteEntity
  , makeComponentSet
  , makeEcs
  , unsafeCreateEntity
  , unsafeCreateGroup
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.List (List)
import Data.List as List
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn4, runEffectFn2, runEffectFn4)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | The entity component system is the core of the library.
-- | This is an opaque type, used by the different helpers provided by this library.
data Ecs :: Row Type -> Type
data Ecs components

-- | A group is what keeps track of the intersection of multiple component managers
data Group :: Row Type -> Type
data Group components

-- | A component manager is the class which manages the lifetime of a particular component
data ComponentManager :: Symbol -> Type -> Type
data ComponentManager name component

data OpaqueComponentManager

-- | Unique id identifying an entity
newtype EntityId :: Type
newtype EntityId = EntityId Int

---------- Configuration options
-- | Opaque type which represents instances of classes which implement [ICache](https://docs.thi.ng/umbrella/ecs/interfaces/ICache.html)
data Cache

-- | A memory mapped component should lead to more efficient iteration speed (?) 
-- | and somewhat easy interop with the gpu
type MemMappedComponent =
  { buffer :: Opt ArrayBuffer
  , byteOffset :: Opt Int
  , size :: Opt Int
  , stride :: Opt Int
  , cache :: Opt Cache
  , type :: String -- TODO: fix this 
  }

-- | An individual component spec specifies how a manager stores it's components
data ComponentSpec :: forall k. k -> Type
data ComponentSpec a
  -- | An object component manager can store any javascript value. Useful when the size of the data is not constant
  = ObjectComponent

  | MemMappedComponent MemMappedComponent

-- | A component set is just a set of configurations 
-- | for the components specified by the given row type.
data ComponentSet :: Row Type -> Type
data ComponentSet components

-- | Opaque type which represents classes implementing [IMemPoolArray](https://docs.thi.ng/umbrella/malloc/interfaces/imempoolarray.html)
data MemPool

-- TODO: add custom object pool setting
-- | The config required to build an ecs
type EcsSpec components =
  {
    -- | The maximum amount of entities in the system at one point in time
    size :: Int

  -- | Optional custom implementation of entity allocation / deallocation
  , pool :: Opt MemPool

  -- | Configuration for all components in the entity component system
  , components :: ComponentSet components
  }

-- | Options for creating a group
type GroupSpec =
  { id :: Opt String
  , cache :: Opt Cache
  }

---------- Component set creation
-- | Typelevel tag for the operation of creating component specs.
-- | Internal to this module. Exporting can lead to unsafe code.
data MakeComponentSet

-- | Instance used to compute the record of component specs at the typelevel
instance Mapping MakeComponentSet n (ComponentSpec n) where
  mapping = unsafeCoerce

-- | Dummy function to remove typeclass constrains from a set of component specs
makeComponentSet
  :: forall components specs
   . HMap MakeComponentSet (Record components) (Record specs)
  => Record specs
  -> ComponentSet components
makeComponentSet = unsafeCoerce

---------- Helpers
createEntity
  :: forall components provided remaining
   . Row.Union provided remaining components
  => Record provided
  -> Ecs components
  -> Effect EntityId
createEntity = runEffectFn2 unsafeCreateEntity

---------- Gruop creation
createGroup
  :: forall components group remaining owned remaining' grl orl
   . Row.Union group remaining components
  => Row.Union owned remaining' components
  => RL.RowToList owned orl
  => RL.RowToList group grl
  => HFoldlWithIndex SelectGroupComponents
       (Ecs components -> List OpaqueComponentManager)
       (RLProxy grl)
       (Ecs components -> List OpaqueComponentManager)
  => HFoldlWithIndex SelectGroupComponents
       (Ecs components -> List OpaqueComponentManager)
       (RLProxy orl)
       (Ecs components -> List OpaqueComponentManager)
  => Proxy group
  -> Proxy owned
  -> GroupSpec
  -> Ecs components
  -> Effect (Group remaining)
createGroup _ _ options ecs = runEffectFn4 unsafeCreateGroup
  ( List.toUnfoldable $ hfoldlWithIndex
      SelectGroupComponents
      (const List.Nil :: Ecs components -> List OpaqueComponentManager)
      _selected
      ecs
  )

  ( List.toUnfoldable $ hfoldlWithIndex
      SelectGroupComponents
      (const List.Nil :: Ecs components -> List OpaqueComponentManager)
      _owned
      ecs
  )
  options
  ecs
  where
  _owned :: RLProxy orl
  _owned = RLProxy

  _selected :: RLProxy grl
  _selected = RLProxy

data SelectGroupComponents = SelectGroupComponents

instance
  ( IsSymbol key
  ) =>
  FoldingWithIndex SelectGroupComponents
    (Proxy key)
    (Ecs components -> List OpaqueComponentManager)
    (Proxy ty)
    (Ecs components -> List OpaqueComponentManager)
  where
  foldingWithIndex _ key previous _ ecs = List.Cons
    (unsafeGetComponentManager name ecs)
    (previous ecs)
    where
    name = reflectSymbol key

---------- Foreign imports
-- | Unsafe version of makeEcs
foreign import makeEcs
  :: forall result components
   . EcsSpec components
  -> (Ecs components -> result)
  -> Effect result

-- | Foreign, unsafe way of adding an entity to an ecs
foreign import unsafeCreateEntity
  :: forall a b
   . EffectFn2
       (Record a)
       (Ecs b)
       EntityId

-- | Unsafe version of createGroup
foreign import unsafeCreateGroup
  :: forall a b
   . EffectFn4
       (Array OpaqueComponentManager)
       (Array OpaqueComponentManager)
       GroupSpec
       (Ecs a)
       (Group b)

-- | Deletes an entity from the system
foreign import deleteEntity :: forall components. EntityId -> Ecs components -> Effect Boolean

foreign import unsafeGetComponentManager :: forall components. String -> Ecs components -> OpaqueComponentManager