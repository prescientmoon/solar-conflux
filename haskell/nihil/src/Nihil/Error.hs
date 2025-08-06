{-# OPTIONS_GHC -Wno-orphans #-}

module Nihil.Error where

import Data.Text qualified as Text
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Types qualified as LSP
import Nihil.Utils (chooseFirst)
import Nihil.Utils qualified as Utils
import Optics qualified as O
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as DG
import Relude
import Text.Megaparsec qualified as M

-- {{{ Paths
type Path = LSP.NormalizedUri

instance IsString LSP.NormalizedUri where
  fromString = LSP.toNormalizedUri . LSP.Uri . Text.pack

instance IsString LSP.Uri where
  fromString = LSP.Uri . Text.pack

pathToString ∷ Path → String
pathToString (LSP.NormalizedUri _ s) = Text.unpack s

-- }}}
-- {{{ Reports & Errors
type Doc = PP.Doc DG.AnsiStyle
type Report = DG.Report Doc
type Diagnostics = DG.Diagnostic Doc
type Span = DG.Position

-- data Severity = Error | Warning
-- data ReportNG = ReportNG
--   { severity ∷ Severity
--   }

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

reportFile ∷ Report → Maybe Path
reportFile r =
  reportMainMarker r
    <&> \(s, _) →
      LSP.toNormalizedUri
        . LSP.Uri
        $ Text.pack s.file

reportMarkers ∷ Report → [(Span, DG.Marker Doc)]
reportMarkers (DG.Warn _ _ markers _) = markers
reportMarkers (DG.Err _ _ markers _) = markers

reportMainMarker ∷ Report → Maybe (Span, Doc)
reportMainMarker r = chooseFirst $ go <$> reportMarkers r
 where
  go (s, DG.This m) = Just (s, m)
  go _ = Nothing

reportCode ∷ Report → Maybe Doc
reportCode (DG.Warn c _ _ _) = c
reportCode (DG.Err c _ _ _) = c

reportDescription ∷ Report → Doc
reportDescription (DG.Warn _ d _ _) = d
reportDescription (DG.Err _ d _ _) = d

-- }}}
-- {{{ Spans & Positions
newtype Pos = Pos M.SourcePos
  deriving (Eq, Show, Generic)

instance Semigroup Span where
  a <> b =
    DG.Position
      { file = O.view #file b
      , begin = min (O.view #begin a) (O.view #begin b)
      , end = max (O.view #end a) (O.view #end b)
      }

isSubspan ∷ Span → Span → Bool
isSubspan small big =
  small.file == big.file
    && small.begin >= big.begin
    && small.end <= big.end

lspPosToPos ∷ Path → LSP.Position → Pos
lspPosToPos source pos =
  Pos $
    M.SourcePos
      { sourceName = pathToString source
      , sourceLine = M.mkPos $ fromIntegral $ pos._line + 1
      , sourceColumn = M.mkPos $ fromIntegral $ pos._character + 1
      }

posToLspPos ∷ Pos → LSP.Position
posToLspPos (Pos sp) =
  LSP.Position
    { _line = fromIntegral $ M.unPos sp.sourceLine - 1
    , _character = fromIntegral $ M.unPos sp.sourceColumn - 1
    }

lspPosToSpan ∷ Path → LSP.Position → Span
lspPosToSpan source = mkSpan' . lspPosToPos source

spanStart ∷ Span → Pos
spanStart s =
  Pos $
    M.SourcePos
      { sourceName = s.file
      , sourceLine = M.mkPos $ fst s.begin
      , sourceColumn = M.mkPos $ snd s.begin
      }

spanEnd ∷ Span → Pos
spanEnd s =
  Pos $
    M.SourcePos
      { sourceName = s.file
      , sourceLine = M.mkPos $ fst s.end
      , sourceColumn = M.mkPos $ snd s.end
      }

spanToLspRange ∷ Span → LSP.Range
spanToLspRange s =
  LSP.Range
    { _start = posToLspPos $ spanStart s
    , _end = posToLspPos $ spanEnd s
    }

spanToLspLocation ∷ Span → LSP.Location
spanToLspLocation s =
  LSP.Location
    { _uri = fromString s.file
    , _range = spanToLspRange s
    }

mkSpan ∷ Pos → Pos → Span
mkSpan from to =
  DG.Position
    { begin =
        ( M.unPos $ M.sourceLine $ coerce from
        , M.unPos $ M.sourceColumn $ coerce from
        )
    , end =
        ( M.unPos $ M.sourceLine $ coerce to
        , let p = M.unPos (M.sourceColumn $ coerce to)
           in if from == to then p + 1 else p
        )
    , file = M.sourceName $ coerce from
    }

mkSpan' ∷ Pos → Span
mkSpan' from = mkSpan from from

-- }}}
