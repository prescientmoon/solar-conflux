module Nihil.Server.State
  ( ServerConfig
  , LspM
  , MonadLsp
  , ELspM
  , Context (..)
  , say
  , getPath
  , getSource
  , getPosition
  , runELspMResponder
  , runELspM
  , getCompilerState
  , putCompilerState
  , runCompilerM
  ) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Text.Mixed.Rope qualified as Rope
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Nihil.Compiler.Monad (CompilerContext, CompilerState, defaultCompilerContext)
import Nihil.Error qualified as Error
import Optics ((%))
import Optics qualified as O
import Relude

-- {{{ The underlying monad
data Context = Context
  { state ∷ IORef CompilerState
  }
  deriving (Generic)

type ServerConfig = ()
type LspM = ReaderT Context (LSP.LspM ServerConfig)

type MonadLsp m =
  ( LSP.MonadLsp ServerConfig m
  , MonadReader Context m
  )

getCompilerState ∷ ∀ m. (MonadLsp m) ⇒ m CompilerState
getCompilerState = do
  st ← asks $ O.view #state
  readIORef st

putCompilerState ∷ ∀ m. (MonadLsp m) ⇒ CompilerState → m ()
putCompilerState cs = do
  st ← asks $ O.view #state
  writeIORef st cs

-- | A concrete instantiation of the @`MonadCompile` class.
type ConcreteCompilerM m = StateT CompilerState (ReaderT CompilerContext m)

runCompilerM ∷ ∀ m a. (MonadLsp m) ⇒ ConcreteCompilerM m a → m a
runCompilerM action = do
  st ← getCompilerState
  (res, st') ← flip runReaderT defaultCompilerContext $ runStateT action st
  putCompilerState st'
  pure res

-- }}}
-- {{{ Communicating with the client
say ∷ Text → LspM ()
say msg = do
  void $
    LSP.sendNotification
      LSP.SMethod_WindowShowMessage
      (LSP.ShowMessageParams LSP.MessageType_Info msg)

sendError ∷ Text → LspM ()
sendError msg = do
  void $
    LSP.sendNotification
      LSP.SMethod_WindowShowMessage
      (LSP.ShowMessageParams LSP.MessageType_Error msg)

-- }}}
-- {{{ Error handling
type ELspM = ExceptT Text LspM

-- | Runs the `@ELspM@ monad, notifying the client of errors. When performing
-- such a step while answering a client's request, `@runELspMResponder@ should
-- be used instead.
runELspM ∷ ELspM () → LspM ()
runELspM compute = do
  res ← runExceptT compute
  case res of
    Right _ → pure ()
    Left message → do
      sendError message

-- | Similar to `@runELspM@, except the error is sent back as the response to
-- the client's request.
runELspMResponder
  ∷ ∀ a msg
   . (Either (LSP.TResponseError msg) a → LspM ())
  → ELspM ()
  → LspM ()
runELspMResponder responder compute = do
  res ← runExceptT compute
  case res of
    Right _ → pure ()
    Left message → do
      responder . Left $
        LSP.TResponseError
          { _xdata = Nothing
          , _message = message
          , _code = LSP.InL LSP.LSPErrorCodes_RequestFailed
          }

-- }}}
-- {{{ Query helpers
type HasPath notif params doc =
  ( LSP.HasParams notif params
  , LSP.HasTextDocument params doc
  , LSP.HasUri doc LSP.Uri
  )

type HasPosition notif params =
  ( LSP.HasParams notif params
  , LSP.HasPosition params LSP.Position
  )

getPath
  ∷ ∀ notif params doc
   . (HasPath notif params doc)
  ⇒ notif
  → Error.Path
getPath notif = do
  -- Lenses
  let _textDocument = O.lensVL LSP.textDocument
  let _uri = O.lensVL LSP.uri

  -- Get the normalized URI
  let params = O.view (O.lensVL LSP.params) notif
  let uri = O.view (_textDocument % _uri) params
  LSP.toNormalizedUri uri

getPosition
  ∷ ∀ notif params doc
   . ( HasPath notif params doc
     , HasPosition notif params
     )
  ⇒ notif
  → Error.Span
getPosition notif = Error.lspPosToSpan path pos
 where
  path = getPath notif
  pos =
    O.view
      ( O.lensVL LSP.params
          % O.lensVL LSP.position
      )
      notif

getSource
  ∷ ∀ notif params doc
   . (HasPath notif params doc)
  ⇒ notif
  → ELspM Text
getSource notif = do
  let uri = getPath notif

  mbContent ← lift $ LSP.getVirtualFile uri
  content ← case mbContent of
    Nothing → throwError "No file content found..."
    Just content → pure content

  pure $ Rope.toText $ O.view (O.lensVL VFS.file_text) content

-- }}}
