module Nihil.Server.Lifecycle
  ( onChangeHandler
  , onOpenHandler
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Nihil.Ast.Lifecycle qualified as Lifecycle
import Nihil.Ast.State qualified as Ast
import Nihil.Error qualified as Error
import Nihil.Server.State qualified as Server
import Nihil.Utils qualified as Utils
import Relude

onChangeHandler ∷ LSP.Handlers Server.LspM
onChangeHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidChange $
  \notif → Server.runELspM do
    let path = Server.getPath notif
    text ← Server.getSource notif

    cs ← lift Server.getCompilerState
    let cs' = flip execState cs $ Lifecycle.onChange path text
    lift $ Server.putCompilerState cs'
    sendDiagnostics

onOpenHandler ∷ LSP.Handlers Server.LspM
onOpenHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $
  \notif → Server.runELspM do
    let path = Server.getPath notif
    text ← Server.getSource notif

    cs ← lift Server.getCompilerState
    let cs' = flip execState cs $ Lifecycle.onChange path text
    lift $ Server.putCompilerState cs'
    sendDiagnostics

sendDiagnostics ∷ Server.ELspM ()
sendDiagnostics = do
  cs ← lift Server.getCompilerState
  let reportMap =
        splitReports (Ast.collectReports cs) $
          mempty <$ cs.files

  -- caps ← lift LSP.getClientCapabilities
  -- pdCaps ← case caps._textDocument >>= (\x → x._publishDiagnostics) of
  --   Nothing → throwError "No semantic token client capability."
  --   Just stCaps → pure stCaps
  -- lift $ Server.say $ "Caps: " <> show pdCaps

  for_ (HashMap.toList reportMap) \(path, reports) → do
    let notification =
          LSP.PublishDiagnosticsParams
            { _diagnostics =
                toList reports & mapMaybe \report → do
                  main ← Error.reportMainMarker report

                  let showMarkerImpl s msg =
                        LSP.DiagnosticRelatedInformation
                          { _location = Error.spanToLspLocation s
                          , _message = Utils.textPretty' msg
                          }

                  let showMarker (_, DG.Blank) = Nothing
                      showMarker (s, DG.This msg) = Just $ showMarkerImpl s msg
                      showMarker (s, DG.Where msg) = Just $ showMarkerImpl s msg
                      showMarker (s, DG.Maybe msg) = Just $ showMarkerImpl s msg

                  pure $
                    LSP.Diagnostic
                      { _source = Just "nihil"
                      , _severity = Just case report of
                          DG.Warn _ _ _ _ → LSP.DiagnosticSeverity_Warning
                          DG.Err _ _ _ _ → LSP.DiagnosticSeverity_Error
                      , _code =
                          Error.reportCode report
                            <&> \doc → LSP.InR $ Utils.textPretty' doc
                      , _codeDescription = Nothing
                      , _message =
                          -- Utils.textPretty' $
                          --   Error.reportDescription report
                          Utils.textPretty' $ snd main
                      , _range = Error.spanToLspRange $ fst main
                      , _relatedInformation =
                          Just $
                            mapMaybe showMarker $
                              Error.reportMarkers report
                      , _tags = Nothing
                      , _data_ = Nothing
                      }
            , _version = Nothing
            , _uri = case path of
                LSP.NormalizedUri _ t → LSP.Uri t
            }

    -- lift $
    --   Server.say $
    --     "pushing diagnostics to "
    --       <> show path
    --       <> ": "
    --       <> show notification

    lift
      . LSP.sendNotification LSP.SMethod_TextDocumentPublishDiagnostics
      $ notification
 where
  splitReports Seq.Empty acc = acc
  splitReports (f :<| rest) acc = do
    case Error.reportFile f of
      Nothing → splitReports rest acc
      Just file →
        splitReports rest $
          HashMap.insertWith (<>) file (pure f ∷ Seq Error.Report) acc
