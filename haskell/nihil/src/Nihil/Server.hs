module Nihil.Server (run) where

import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Options (..), ServerDefinition (..), defaultOptions)
import Language.LSP.Server qualified as LSP
import Nihil.Ast.State (initialCompilerState)
import Nihil.Server.Lifecycle qualified as Lifecycle
import Nihil.Server.Name qualified as Names
import Nihil.Server.SemanticTokens qualified as SemanticTokens
import Nihil.Server.State (ServerConfig)
import Nihil.Server.State qualified as Server
import Relude

run ∷ IO Int
run = do
  s ← newIORef initialCompilerState
  LSP.runServer . serverDefinition $
    Server.Context
      { state = s
      }

serverDefinition ∷ Server.Context → ServerDefinition ServerConfig
serverDefinition ctx =
  ServerDefinition
    { defaultConfig = ()
    , configSection = "nihil"
    , parseConfig = \_ _ → pure ()
    , onConfigChange = \_ → pure ()
    , doInitialize = \env _ → pure $ Right env
    , options = options
    , interpretHandler = \env →
        LSP.Iso
          (LSP.runLspT env . flip runReaderT ctx)
          liftIO
    , staticHandlers = \_caps → handlers
    }
 where
  options ∷ Options
  options =
    defaultOptions
      { optTextDocumentSync =
          Just $
            LSP.TextDocumentSyncOptions
              { LSP._openClose = Just True
              , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
              , LSP._willSave = Just False
              , LSP._willSaveWaitUntil = Just False
              , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
              }
      }

handlers ∷ LSP.Handlers Server.LspM
handlers =
  fold
    [ LSP.notificationHandler LSP.SMethod_Initialized $ \_not → do
        pure ()
    , LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ \_notif → do
        pure ()
    , Lifecycle.onChangeHandler
    , Lifecycle.onOpenHandler
    , SemanticTokens.handler
    , Names.gotoHandler
    ]
