module Nihil.Parser.Module (module') where

import Relude

import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Module
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Notation qualified as Core
import Text.Megaparsec qualified as M

module' ∷ Core.Parser Module
module' = do
  pos ← M.getSourcePos
  mbModule' ← Core.unindented $ M.optional $ snd $ Core.string "module"
  (name, exports, where') ← case mbModule' of
    Nothing → do
      Core.reportError
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

  (decls, mbEof) ←
    Core.anyIndentation $
      Core.manyTill
        (Core.label "declaration" . Core.unindented $ decl)
        (Core.label "end of file" . Core.anyIndentation $ Core.token M.eof)

  eof ← case mbEof of
    Just e → pure e
    Nothing →
      Core.anyIndentation
        . Core.junkTill
        . Core.label "end of file"
        $ Core.token M.eof

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
  tok ← snd $ Core.string "hi"
  -- void
  --   . Core.separated False (Core.string ",")
  --   $ Core.label "name" Core.name
  pure $ DeclValue $ Value (Just tok) Nothing Nothing []
