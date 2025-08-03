module Nihil.Server (run) where

import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Options (..), ServerDefinition (..), defaultOptions)
import Language.LSP.Server qualified as LSP
import Nihil.Server.SemanticTokens qualified as SemanticTokens
import Nihil.Server.State (ServerConfig)
import Relude

run ∷ IO Int
run = LSP.runServer serverDefinition

serverDefinition ∷ ServerDefinition ServerConfig
serverDefinition =
  ServerDefinition
    { defaultConfig = ()
    , configSection = "nihil"
    , parseConfig = \_ _ → pure ()
    , onConfigChange = \_ → pure ()
    , doInitialize = \env _ → pure $ Right env
    , options = options
    , interpretHandler = \env → LSP.Iso (LSP.runLspT env) liftIO
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

handlers ∷ LSP.Handlers (LSP.LspM ())
handlers =
  fold
    [ LSP.notificationHandler LSP.SMethod_Initialized $ \_not → do
        pure ()
    , LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ \_notif → do
        pure ()
    , LSP.notificationHandler LSP.SMethod_TextDocumentDidChange $ \_notif → do
        pure ()
    , LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ \_notif → do
        pure ()
    , SemanticTokens.handler
    ]
