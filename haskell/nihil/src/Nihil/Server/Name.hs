-- | Implements things like goto-definition, hovering, etc
module Nihil.Server.Name where

import Control.Monad.Cont (ContT (runContT), MonadCont (callCC), evalContT)
import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Nihil.Ast.State qualified as Ast
import Nihil.Cst.Base qualified as Cst
import Nihil.Error qualified as Error
import Nihil.Server.State (LspM)
import Nihil.Utils (chooseFirstM)
import Optics ((%))
import Optics qualified as O
import Relude
import Text.Megaparsec qualified as M

posToSourcePos ∷ Text → LSP.Position → Error.Pos
posToSourcePos sourceName p =
  Error.Pos $
    M.SourcePos
      { sourceName = Text.unpack sourceName
      , sourceLine = M.mkPos $ fromIntegral $ p._line + 1
      , sourceColumn = M.mkPos $ fromIntegral $ p._character + 1
      }

posToSpan ∷ Text → LSP.Position → Error.Span
posToSpan sourceName =
  Cst.mkMegaparsecSpan'
    . coerce
    . posToSourcePos sourceName

nameFindBinder
  ∷ ∀ m
   . (MonadState Ast.AstState m)
  ⇒ Error.Span
  → Ast.Name
  → Ast.NodeId
  → m (Maybe (Ast.NodeId, Ast.Name))
nameFindBinder pos name i = do
  Ast.getSpan i >>= \case
    Just s | pos `Error.isSubspan` s → pure $ Just (i, name)
    _ → pure Nothing

typeFindBinder
  ∷ ∀ m
   . (MonadState Ast.AstState m)
  ⇒ Error.Span
  → Ast.Type'
  → m (Maybe (Ast.NodeId, Ast.Name))
typeFindBinder pos (Ast.TyVar i name) =
  nameFindBinder pos name i
typeFindBinder pos (Ast.TyLambda _i _scope _binder ty) =
  chooseFirstM
    [ -- TODO: allow hovering the lambda argument itself
      typeFindBinder pos ty
    ]
typeFindBinder pos (Ast.TyForall _i _scope _binder ty) =
  chooseFirstM
    [ -- TODO: allow hovering the quantified variable itself
      typeFindBinder pos ty
    ]
typeFindBinder pos (Ast.TyArrow _ _ from to) =
  -- TODO: highlighting the arrow symbol
  chooseFirstM
    [ typeFindBinder pos from
    , typeFindBinder pos to
    ]
typeFindBinder pos (Ast.TyApp _ f a) =
  chooseFirstM
    [ typeFindBinder pos f
    , typeFindBinder pos a
    ]
typeFindBinder _ (Ast.TyUnknown _) = pure Nothing

exprDefinitionFindBinder
  ∷ ∀ m
   . (MonadState Ast.AstState m)
  ⇒ Error.Span
  → Ast.Name
  → Ast.ExprDefinition
  → m (Maybe (Ast.NodeId, Ast.Name))
exprDefinitionFindBinder pos name (Ast.EParam i) =
  nameFindBinder pos name i
exprDefinitionFindBinder pos name (Ast.EForeign i ty) =
  chooseFirstM
    [ nameFindBinder pos name i
    , typeFindBinder pos ty
    ]
exprDefinitionFindBinder pos _name (Ast.EDeclaration _i ty _expr) =
  chooseFirstM
    [ -- TODO: allow hovering the declaration symbol
      typeFindBinder pos ty
    ]

findBinder ∷ Error.Path → Error.Span → Ast.AstState → Maybe (Ast.NodeId, Ast.Name)
findBinder path pos = evalState $ evalContT $ callCC \escape → do
  scopes ← O.preuse (#files % O.ix path % #scopes)

  -- TODO: only iterate over the top-level scope in each file.
  for_ (fold scopes) \i →
    O.preuse (#scopes % O.ix i) >>= traverse_ \scope → do
      for_ (HashMap.toList scope.exprs) \(name, expr) → do
        exprDefinitionFindBinder pos (Ast.Resolved i name) expr
          >>= traverse_ \res → escape $ Just res

  pure Nothing

gotoHandler ∷ LSP.Handlers LspM
gotoHandler = LSP.requestHandler
  LSP.SMethod_TextDocumentDefinition
  \notif responder → do
    let params = O.view (O.lensVL LSP.params) notif
    let uri = O.view (#_textDocument % #_uri) params
    let nUri@(LSP.NormalizedUri _ fileName) = LSP.toNormalizedUri uri
    let s = posToSpan fileName params._position

    pure ()
