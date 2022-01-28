module Sky.Language.MetaVar where

import Prelude

import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Run (Run)
import Run.State (State)
import Run.State as State
import Sky.Language.Error (MetaError(..), SKY_ERROR, throwMetaError)
import Sky.Language.Term (MetaContext(..), MetaVar, Value)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Effect related stuff
type META_CONTEXT a r =
  ( metaContext :: State (MetaContext a)
  | r
  )

getMetaContext :: forall a r. Run (META_CONTEXT a r) (MetaContext a)
getMetaContext = State.getAt _metaContext

---------- Helpers
lookupMeta
  :: forall a r
   . a
  -> MetaVar
  -> Run (SKY_ERROR a + META_CONTEXT a + r) (Maybe (Value a))
lookupMeta source var = getMetaContext >>= \(MetaContext context) ->
  case HM.lookup var context of
    Just metaEntry -> pure metaEntry
    Nothing -> throwMetaError $ MetaNotInContext
      { meta: var
      , source
      }

---------- Proxies
_metaContext :: Proxy "metaContext"
_metaContext = Proxy