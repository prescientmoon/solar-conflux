module Nihil.Parser.Module (module') where

import Relude

import Data.Set qualified as Set
import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Module
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Notation qualified as Core
import Text.Megaparsec qualified as M

module' ∷ Core.Parser Module
module' = Core.exactlyIndented do
  block ← Core.blockLike
  offset ← M.getOffset
  pos ← M.getSourcePos
  mbModule' ← Core.mkBlock block $ M.optional $ M.try $ snd $ Core.string "module"
  -- (name, exports, where') ← pure (Nothing, Nothing, Nothing)
  (name, exports, where') ← case mbModule' of
    Nothing → do
      Core.reportError
        offset
        "NoModuleHeader"
        "No module header found."
        [(Base.mkMegaparsecSpan' pos, DG.This "This is where I thought the module header might be.")]
        [] -- TODO: suggest fix based on file name
      pure (Nothing, Nothing, Nothing)
    Just _ → Core.tighten Core.do
      name ← Core.step $ Core.label "module name" Core.name
      exports' ←
        Core.optStep
          $ Core.label "export list"
          $ Core.delimitedList
            (Core.string "(")
            (Core.string ")")
            (Core.string ",")
          $ Core.label "export name" Core.name
      where' ← Core.pre $ Core.step $ Core.string "where"
      Core.pure (name, exports', where')

  -- let (decls, mbEof) = (mempty, Nothing)
  (decls, mbEof) ←
    Core.manyTill
      (Core.label "declaration" $ Core.mkBlock block decl)
      (second (fmap void) $ Core.string "struct")
  eof ← case mbEof of
    Nothing → Core.resetStopOn $ Core.junkTill ("end of file", Core.token M.eof)
    Just e → pure e

  pure $
    Module
      { module' = mbModule'
      , name = name
      , exports = exports
      , where' = where'
      , decls = decls
      , eof = eof
      }

decl ∷ Core.Parser Declaration
decl = do
  void
    . Core.separated False (Core.string ",")
    $ Core.label "name" Core.name
  pure $ DeclValue $ Value Nothing Nothing Nothing []
