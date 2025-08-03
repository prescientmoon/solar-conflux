module Nihil.Server.State (ServerConfig, LspM, say) where

import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Nihil.Cst.Module qualified as Cst
import Nihil.Error (Report)
import Relude

type ServerConfig = ()
type LspM = LSP.LspM ServerConfig
type Path = LSP.Uri

data FileState = FileState
  { cst ∷ Cst.Module
  , reports ∷ Seq Report
  }

data ServerState = ServerState
  { files ∷ HashMap Path FileState
  }

say ∷ Text → LspM ()
say msg = do
  void $
    LSP.sendNotification
      LSP.SMethod_WindowShowMessage
      (LSP.ShowMessageParams LSP.MessageType_Info msg)
