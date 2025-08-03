module Nihil.Server.SemanticTokens (handler, semanticTokens) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Mixed.Rope qualified as Rope
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Nihil.Cst.Base qualified as Cst
import Nihil.Cst.Expr qualified as Cst.Expr
import Nihil.Cst.Module qualified as Cst
import Nihil.Cst.Type qualified as Cst.Type
import Nihil.Parser.Core qualified as Parser
import Nihil.Parser.Module qualified as Parser
import Nihil.Server.State (LspM)
import Optics ((%))
import Optics qualified as O
import Relude

-- {{{ Handler
handler ∷ LSP.Handlers LspM
handler = LSP.requestHandler
  LSP.SMethod_TextDocumentSemanticTokensFull
  \notif responder → do
    let
      respondErr ∷ Text → LspM ()
      respondErr message =
        responder . Left $
          LSP.TResponseError
            { _xdata = Nothing
            , _message = message
            , _code = LSP.InL LSP.LSPErrorCodes_RequestFailed
            }

    let params = O.view (O.lensVL LSP.params) notif
    let uri = O.view (#_textDocument % #_uri) params
    let nUri = LSP.toNormalizedUri uri

    res ← runExceptT do
      caps ← lift LSP.getClientCapabilities
      stCaps ← case caps._textDocument >>= (\x → x._semanticTokens) of
        Nothing → throwError "No semantic token client capability."
        Just stCaps → pure stCaps

      let legend =
            LSP.SemanticTokensLegend
              { _tokenTypes = stCaps._tokenTypes
              , _tokenModifiers = stCaps._tokenModifiers
              }

      mbContent ← lift $ LSP.getVirtualFile nUri
      content ← case mbContent of
        Nothing → throwError "No file content found..."
        Just content → pure content

      let text = Rope.toText $ O.view (O.lensVL VFS.file_text) content
      let (_, mbParsed) =
            Parser.runParser
              Parser.pModule
              (Text.unpack $ O.view #getUri uri)
              text

      module' ← case mbParsed of
        Nothing → throwError "Failed to parse module..."
        Just (module', _) → pure module'

      let trivia = fold $ O.view (O.partsOf Cst.allTrivia) module'

      let getPos t =
            ( O.view (O.lensVL LSP.line) t
            , O.view (O.lensVL LSP.startChar) t
            )

      let hasValidBaseType t =
            elem
              (LSP.toEnumBaseType (O.view tokenType t))
              legend._tokenTypes

      let absoluteToks =
            sortOn getPos
              . filter hasValidBaseType
              . toList
              $ fold
                [ semanticTokens module'
                , trivia >>= triviaToSemantic
                ]

      case LSP.makeSemanticTokens legend absoluteToks of
        Right toks → lift . responder . Right $ LSP.InL toks
        Left err →
          throwError $
            "An error ocurred while trying to generate the tokens: "
              <> err

    case res of
      Right _ → pure ()
      Left err → respondErr err

-- }}}

-- {{{ The HasSemanticTokens typeclass
class HasSemanticTokens a where
  semanticTokens ∷ a → Seq LSP.SemanticTokenAbsolute

mkTokens
  ∷ LSP.SemanticTokenTypes
  → [LSP.SemanticTokenModifiers]
  → Cst.Span
  → Seq LSP.SemanticTokenAbsolute
mkTokens ty mods (DG.Position{..})
  | fst begin == fst end =
      -- TODO: trivia
      pure $
        LSP.SemanticTokenAbsolute
          { _tokenType = ty
          , _startChar = fromIntegral $ snd begin - 1
          , _length = fromIntegral $ snd end - snd begin
          , _tokenModifiers = mods
          , _line = fromIntegral $ fst begin - 1
          }
  | otherwise = mempty

toSemantic
  ∷ ∀ a
   . LSP.SemanticTokenTypes
  → [LSP.SemanticTokenModifiers]
  → Cst.Token a
  → Seq LSP.SemanticTokenAbsolute
toSemantic ty mods tok =
  fold
    [ mkTokens ty mods (O.view #span tok)
    ]

triviaToSemantic ∷ Cst.Trivia → Seq LSP.SemanticTokenAbsolute
triviaToSemantic (Cst.TJunk _ _) = mempty
triviaToSemantic (Cst.TComment s _) =
  mkTokens "comment" [] s

-- }}}
-- {{{ Manual instances
instance HasSemanticTokens Cst.Module where
  semanticTokens (Cst.Module{..}) =
    join . Seq.fromList $
      [ fold $ toSemantic "keyword" [] <$> module'
      , fold $ toSemantic "namespace" [] <$> name
      , O.set tokenType "variable" <$> semanticTokens exports
      , fold $ toSemantic "keyword" [] <$> where'
      , semanticTokens decls
      , toSemantic "eof" [] eof
      ]

instance HasSemanticTokens Cst.Declaration where
  semanticTokens (Cst.DeclIndLike i) = semanticTokens i
  semanticTokens (Cst.DeclTypeAlias i) = semanticTokens i
  semanticTokens (Cst.DeclForeignType i) = semanticTokens i
  semanticTokens (Cst.DeclForeignValue i) = semanticTokens i
  semanticTokens (Cst.DeclValueTypeAnn i) = semanticTokens i
  semanticTokens (Cst.DeclValueEquation i) = semanticTokens i

instance HasSemanticTokens Cst.IndLike where
  semanticTokens _ = mempty

instance HasSemanticTokens Cst.TypeAlias where
  semanticTokens (Cst.TypeAlias{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] ty
      , fold $ toSemantic "type" ["declaration"] <$> name
      , fold $ toSemantic "typeParameter" [] <$> args
      , fold $ semanticTokens <$> body
      ]

instance HasSemanticTokens Cst.ForeignValue where
  semanticTokens (Cst.ForeignValue{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] foreign'
      , fold $ toSemantic "function" ["declaration"] <$> name
      , fold $ semanticTokens <$> ty
      ]

instance HasSemanticTokens Cst.ForeignType where
  semanticTokens (Cst.ForeignType{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] foreign'
      , toSemantic "keyword" [] ty
      , fold $ toSemantic "type" [] <$> name
      , O.set tokenType "typeParameter" <$> semanticTokens args
      ]

instance HasSemanticTokens Cst.ValueTypeAnnotation where
  semanticTokens (Cst.ValueTypeAnnotation{..}) =
    join . Seq.fromList $
      [ toSemantic "function" ["declaration"] name
      , fold $ semanticTokens <$> ty
      ]

instance HasSemanticTokens Cst.ValueEquation where
  semanticTokens (Cst.ValueEquation{..}) =
    join . Seq.fromList $
      [ toSemantic "function" ["definition"] name
      , fold $ semanticTokens <$> args
      , fold $ semanticTokens <$> expr
      ]

instance HasSemanticTokens Cst.Type.Type' where
  semanticTokens (Cst.Type.TyForall x) = semanticTokens x
  semanticTokens (Cst.Type.TyVar x) = semanticTokens x
  semanticTokens (Cst.Type.TyParens x) = semanticTokens x
  semanticTokens (Cst.Type.TyApp x) = semanticTokens x
  semanticTokens (Cst.Type.TyArrow x) = semanticTokens x

instance HasSemanticTokens Cst.Type.Forall where
  semanticTokens (Cst.Type.Forall{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] tForall
      , fold $ toSemantic "typeParameter" [] <$> names
      , fold $ semanticTokens <$> ty
      ]

instance HasSemanticTokens Cst.Type.Var where
  semanticTokens (Cst.Type.Var{..}) = toSemantic "type" [] name

instance HasSemanticTokens Cst.Type.App where
  semanticTokens (Cst.Type.App{..}) = semanticTokens f <> semanticTokens a

instance HasSemanticTokens Cst.Type.Arrow where
  semanticTokens (Cst.Type.Arrow{..}) =
    fold
      [ semanticTokens from
      , toSemantic "keyword" [] kind
      , semanticTokens to
      ]

instance HasSemanticTokens Cst.Expr.Expr where
  semanticTokens (Cst.Expr.EVar x) = semanticTokens x
  semanticTokens (Cst.Expr.EApp x) = semanticTokens x
  semanticTokens (Cst.Expr.ELambda x) = semanticTokens x
  semanticTokens (Cst.Expr.EParens x) = semanticTokens x
  semanticTokens (Cst.Expr.EMatch x) = semanticTokens x
  semanticTokens _ = mempty

instance HasSemanticTokens Cst.Expr.Var where
  semanticTokens (Cst.Expr.Var x) = toSemantic "variable" [] x

instance HasSemanticTokens Cst.Expr.App where
  semanticTokens (Cst.Expr.App{..}) = semanticTokens f <> semanticTokens a

instance HasSemanticTokens Cst.Expr.Lambda where
  semanticTokens (Cst.Expr.Lambda{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] lam
      , fold $ semanticTokens <$> patterns
      , fold $ toSemantic "keyword" [] <$> arrow
      , fold $ semanticTokens <$> body
      ]

instance HasSemanticTokens Cst.Expr.Match where
  semanticTokens (Cst.Expr.Match{..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] kind
      , semanticTokens exprs
      , fold $ toSemantic "keyword" [] <$> where'
      , semanticTokens branches
      ]

instance HasSemanticTokens Cst.Expr.MatchBranch where
  semanticTokens (Cst.Expr.MatchBranch{..}) =
    join . Seq.fromList $
      [ semanticTokens patterns
      , fold $ toSemantic "keyword" [] <$> arrow
      , fold $ semanticTokens <$> body
      ]

instance HasSemanticTokens Cst.Expr.Pattern where
  semanticTokens (Cst.Expr.PName x) = toSemantic "parameter" [] x
  semanticTokens (Cst.Expr.PWildcard x) = toSemantic "keyword" [] x
  semanticTokens (Cst.Expr.PProj x) = semanticTokens x
  semanticTokens (Cst.Expr.PParens x) = semanticTokens x

instance HasSemanticTokens Cst.Expr.PatternProj where
  semanticTokens (Cst.Expr.PatternProj{head = head', ..}) =
    join . Seq.fromList $
      [ toSemantic "keyword" [] dot
      , fold $ toSemantic "property" [] <$> head'
      , fold $ semanticTokens <$> args
      ]

instance HasSemanticTokens (Cst.Token a) where
  semanticTokens t = toSemantic "unknown" [] t

instance (HasSemanticTokens a) ⇒ HasSemanticTokens (Maybe a) where
  semanticTokens Nothing = mempty
  semanticTokens (Just x) = semanticTokens x

instance (HasSemanticTokens a) ⇒ HasSemanticTokens (Seq a) where
  semanticTokens = (>>= semanticTokens)

instance (HasSemanticTokens a) ⇒ HasSemanticTokens (Cst.Delimited a) where
  semanticTokens (Cst.Delimited{..}) = semanticTokens inner

instance (HasSemanticTokens a) ⇒ HasSemanticTokens (Cst.Separated sep a) where
  semanticTokens (Cst.Separated{..}) = do
    e ← elements
    case e of
      Left _ → mempty
      Right a → semanticTokens a

-- }}}

-- {{{ Helpers & lenses
-- onUnknowns
--   ∷ (LSP.SemanticTokenAbsolute → LSP.SemanticTokenAbsolute)
--   → (LSP.SemanticTokenAbsolute → LSP.SemanticTokenAbsolute)
-- onUnknowns f t
--   | O.view tokenType t == "unknown" = f t
--   | otherwise = t

tokenType ∷ O.Lens' LSP.SemanticTokenAbsolute LSP.SemanticTokenTypes
tokenType = O.lensVL LSP.tokenType

-- tokenMods ∷ O.Lens' LSP.SemanticTokenAbsolute [LSP.SemanticTokenModifiers]
-- tokenMods = O.lensVL LSP.tokenModifiers

-- }}}
