-- | Implements things like goto-definition, hovering, etc
module Nihil.Server.Name where

import Control.Monad.Cont (MonadCont (callCC), evalContT)
import Data.HashMap.Strict qualified as HashMap
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Nihil.Compiler.Ast qualified as Ast
import Nihil.Compiler.Monad (defaultCompilerContext)
import Nihil.Compiler.Monad qualified as Compiler
import Nihil.Compiler.Resolve qualified as Resolve
import Nihil.Error qualified as Error
import Nihil.Server.State (LspM)
import Nihil.Server.State qualified as Server
import Nihil.Utils (chooseFirstM)
import Optics ((%))
import Optics qualified as O
import Relude

data BinderLink = BinderLink
  { from ∷ Ast.NodeId
  , nameDef ∷ Ast.NodeId
  , decl ∷ Ast.NodeId
  , name ∷ Ast.Name
  }
  deriving (Generic, Show)

-- Find binders
-- {{{ Name
nameFindBinder
  ∷ ∀ m k is a
   . ( Compiler.MonadCompile m
     , Ast.IsNode a
     , O.Is k O.An_AffineFold
     )
  ⇒ (Text → O.Optic' k is Ast.Scope (Maybe (Ast.Definition a)))
  → Error.Span
  → Ast.Name
  → m (Maybe BinderLink)
nameFindBinder at pos name = do
  Compiler.getSpan (Ast.nodeId name) >>= \case
    Just s | pos `Error.isSubspan` s → do
      mbDecl ← Resolve.getUnscopedName at name
      case mbDecl of
        Nothing → pure Nothing
        Just (Ast.Definition names inner) →
          pure $
            Just $
              BinderLink
                { from = Ast.nodeId name
                , name = name
                , nameDef = head names
                , decl = Ast.nodeId inner
                }
    _ → pure Nothing

-- }}}
-- {{{ Type
-- TODO: stop early by looking at the overarching spans attached higher up the
-- tree. Right now, we traverse all the way down the tree, even if warning
-- signs were already there that this would be pointless.
typeFindBinder
  ∷ ∀ m
   . (Compiler.MonadCompile m)
  ⇒ Error.Span
  → Ast.Type'
  → m (Maybe BinderLink)
typeFindBinder pos (Ast.TyVar _ name) =
  nameFindBinder Ast.atTypeInScope pos name
typeFindBinder pos (Ast.TyLambda _ _ binder ty) =
  chooseFirstM
    [ nameFindBinder Ast.atTypeInScope pos binder
    , typeFindBinder pos ty
    ]
typeFindBinder pos (Ast.TyForall _ _ binder ty) =
  chooseFirstM
    [ nameFindBinder Ast.atTypeInScope pos binder
    , typeFindBinder pos ty
    ]
typeFindBinder pos (Ast.TyArrow _ _ from to) =
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

-- }}}
-- {{{ Expression
exprFindBinder
  ∷ ∀ m
   . (Compiler.MonadCompile m)
  ⇒ Error.Span
  → Ast.Expr
  → m (Maybe BinderLink)
exprFindBinder pos (Ast.EVar _ name) =
  nameFindBinder Ast.atExprInScope pos name
exprFindBinder pos (Ast.EMatch _ exprs branches) =
  chooseFirstM
    [ chooseFirstM $ toList $ exprFindBinder pos <$> exprs
    , chooseFirstM
        . toList
        $ ( \(patterns, _, e) →
              chooseFirstM
                [ chooseFirstM . toList $
                    patternFindBinder pos
                      <$> patterns
                , exprFindBinder pos e
                ]
          )
          <$> branches
    ]
exprFindBinder pos (Ast.EApp _ f a) =
  chooseFirstM
    [ exprFindBinder pos f
    , exprFindBinder pos a
    ]
exprFindBinder _ (Ast.EUnknown _) = pure Nothing
exprFindBinder _ _ = error "unimplemented"

patternFindBinder
  ∷ ∀ m
   . (Compiler.MonadCompile m)
  ⇒ Error.Span
  → Ast.Pattern
  → m (Maybe BinderLink)
patternFindBinder _ (Ast.PWildcard _) = pure Nothing
patternFindBinder pos (Ast.PName _ binder) =
  nameFindBinder Ast.atExprInScope pos binder
patternFindBinder pos (Ast.PProj _ _ args) =
  chooseFirstM . toList $ patternFindBinder pos <$> args

-- }}}
-- {{{ Definition
exprDefinitionFindBinder
  ∷ ∀ m
   . (Compiler.MonadCompile m)
  ⇒ Error.Span
  → Ast.ExprDefinition
  → m (Maybe BinderLink)
exprDefinitionFindBinder _ (Ast.EParam _) = pure Nothing
exprDefinitionFindBinder pos (Ast.EForeign _ ty) = typeFindBinder pos ty
exprDefinitionFindBinder pos (Ast.EDeclaration _ ty expr) =
  chooseFirstM
    [ typeFindBinder pos ty
    , exprFindBinder pos expr
    ]

typeDefinitionFindBinder
  ∷ ∀ m
   . (Compiler.MonadCompile m)
  ⇒ Error.Span
  → Ast.TypeDefinition
  → m (Maybe BinderLink)
typeDefinitionFindBinder _ (Ast.TParam _) = pure Nothing
typeDefinitionFindBinder pos (Ast.TForeign _ ty) = typeFindBinder pos ty
typeDefinitionFindBinder pos (Ast.TAlias _ value) =
  chooseFirstM [typeFindBinder pos value]

-- }}}
-- {{{ File
findBinder
  ∷ Error.Path
  → Error.Span
  → Compiler.CompilerState
  → Maybe BinderLink
findBinder path pos st = flip evalState st
  . flip runReaderT defaultCompilerContext
  . evalContT
  $ callCC \escape → do
    O.preuse (#files % O.ix path % #mainScope % O._Just) >>= traverse_ \mainScope →
      O.preuse (#scopes % O.ix mainScope) >>= traverse_ \scope → do
        for_ (HashMap.toList scope.types) \(name, (Ast.Definition nis ty)) → do
          chooseFirstM
            [ typeDefinitionFindBinder pos ty
            , chooseFirstM $
                ( \ni →
                    nameFindBinder Ast.atTypeInScope pos $
                      Ast.Resolved ni mainScope name
                )
                  <$> toList nis
            ]
            >>= traverse_ \res → escape $ Just res

        for_ (HashMap.toList scope.exprs) \(name, (Ast.Definition nis expr)) → do
          chooseFirstM
            [ exprDefinitionFindBinder pos expr
            , chooseFirstM $
                ( \ni →
                    nameFindBinder Ast.atExprInScope pos $
                      Ast.Resolved ni mainScope name
                )
                  <$> toList nis
            ]
            >>= traverse_ \res → escape $ Just res

    pure Nothing

-- }}}

-- Handlers
-- {{{ GOTO definition
gotoHandler ∷ LSP.Handlers LspM
gotoHandler = LSP.requestHandler
  LSP.SMethod_TextDocumentDefinition
  \notif responder → Server.runELspM do
    let path = Server.getPath notif

    cs ← lift Server.getCompilerState
    let pos = Server.getPosition notif
    case findBinder path pos cs of
      Just binder
        | Just sFrom ← evalState (Compiler.getSpan binder.from) cs
        , Just sDecl ← evalState (Compiler.getSpan binder.decl) cs
        , Just sName ← evalState (Compiler.getSpan binder.nameDef) cs → do
            lift
              . responder
              . Right
              . LSP.InR
              . LSP.InL
              . pure
              . LSP.DefinitionLink
              $ LSP.LocationLink
                { _originSelectionRange =
                    Just $ Error.spanToLspRange sFrom
                , _targetUri = LSP.filePathToUri sDecl.file
                , _targetRange = Error.spanToLspRange sDecl
                , _targetSelectionRange = Error.spanToLspRange sName
                }
      _ →
        lift . responder . Right . LSP.InR . LSP.InR $ LSP.Null

-- }}}
