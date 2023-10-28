module PF.Core.Data.Ast where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.RWS (modify)
import Control.Monad.State (class MonadState)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), maybe)
import PF.Core.Data.Cst as Cst
import Safe.Coerce (coerce)

newtype VarId = VarId Int
newtype DBIndex = DBIndex Int
newtype DBLevel = DBLevel Int
newtype DBDepth = DBDepth Int

type Expression = Expression_

type VarInfo =
  { id :: VarId
  }

data Expression_
  = Lambda VarInfo Expression
  | Var VarInfo DBIndex
  | Let VarInfo Expression Expression
  | Call Expression Expression
  | Pi
      { domain :: Expression
      , codomain :: Expression
      , var :: Maybe VarInfo
      }
  | Annotate
      { value :: Expression
      , type_ :: Expression
      }
  | Type

type DesugarContext =
  { scope ::
      HashMap Cst.VarName
        { id :: VarId
        , level :: DBLevel
        }
  , depth :: DBDepth
  }

-- | For depth n 
-- | 0 => n - 0
-- | 1 => n - 1
-- | ...
-- | n - 1 => 1
-- | n - 0=> 0
levelToIndex :: DBDepth -> DBLevel -> DBIndex
levelToIndex (DBDepth depth) (DBLevel level) =
  DBIndex (depth - level)

-- | Inverse of levelToIndex
indexToLevel :: DBDepth -> DBIndex -> DBLevel
indexToLevel (DBDepth depth) (DBIndex index) =
  DBLevel (depth - index)

increaseDepth :: DBDepth -> DBDepth
increaseDepth (DBDepth a) = DBDepth (a + 1)

depthToLevel :: DBDepth -> DBLevel
depthToLevel = coerce

type DesugarState = Int

genId :: forall m. MonadState DesugarState m => m VarId
genId = modify ((+) 1) # map coerce

data DesugarError = VarNotFound Cst.VarName

fromCST
  :: forall m
   . MonadState DesugarState m
  => MonadThrow DesugarError m
  => DesugarContext
  -> Cst.Expression
  -> m Expression
fromCST context = case _ of
  Cst.Type -> pure Type

  Cst.Call f a -> ado
    f <- fromCST context f
    a <- fromCST context a
    in Call f a

  Cst.Annotate { value, type_ } -> ado
    value <- fromCST context value
    type_ <- fromCST context type_
    in Annotate { value, type_ }

  Cst.Var name -> case HashMap.lookup name context.scope of
    Just { id, level } -> do
      let index = levelToIndex context.depth level
      pure $ Var { id } index
    Nothing -> throwError (VarNotFound name)

  Cst.Lambda arg body -> do
    id <- genId
    let extended = extendContext arg id context

    body <- fromCST extended body

    pure $ Lambda { id } body

  Cst.Let var value body -> do
    id <- genId
    let extended = extendContext var id context

    value <- fromCST context value
    body <- fromCST extended body

    pure $ Let { id } value body

  Cst.Pi { var, domain, codomain } -> do
    id <- genId

    let
      extended = var # maybe context
        \var -> extendContext var id context

    domain <- fromCST context domain
    codomain <- fromCST extended codomain

    pure $ Pi
      { domain
      , codomain
      , var: var $> { id }
      }

  where
  extendContext :: Cst.VarName -> VarId -> DesugarContext -> DesugarContext
  extendContext var id context = do
    let depth = increaseDepth context.depth
    { depth: depth
    , scope: HashMap.insert var
        { id
        , level: depthToLevel depth
        }
        context.scope
    }
