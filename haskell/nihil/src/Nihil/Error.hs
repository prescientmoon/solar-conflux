{-# OPTIONS_GHC -Wno-orphans #-}

module Nihil.Error where

import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Types qualified as LSP
import Nihil.Utils qualified as Utils
import Optics qualified as O
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as DG
import Relude
import Text.Megaparsec qualified as M

type Path = LSP.NormalizedUri

data ReportKind = Error | Warning
type Doc = PP.Doc DG.AnsiStyle
type Report = DG.Report Doc
type Diagnostics = DG.Diagnostic Doc
type Span = DG.Position

addReports ∷ ∀ a. [DG.Report a] → DG.Diagnostic a
addReports = foldl' DG.addReport mempty

printDiagnostic
  ∷ ∀ m
   . (MonadIO m)
  ⇒ Diagnostics
  → m ()
printDiagnostic =
  DG.printDiagnostic'
    DG.stdout
    DG.WithUnicode
    (DG.TabSize 2)
    DG.defaultStyle

showDiagnostic ∷ Diagnostics → Text
showDiagnostic =
  Utils.textPretty'
    . PP.unAnnotate
    . DG.prettyDiagnostic' DG.WithUnicode (DG.TabSize 2)

instance Semigroup Span where
  a <> b =
    DG.Position
      { file = O.view #file b
      , begin = min (O.view #begin a) (O.view #begin b)
      , end = max (O.view #end a) (O.view #end b)
      }

newtype Pos = Pos M.SourcePos
  deriving (Show, Generic)

isSubspan ∷ Span → Span → Bool
isSubspan small big =
  small.file == big.file
    && small.begin >= big.begin
    && small.end <= big.end
