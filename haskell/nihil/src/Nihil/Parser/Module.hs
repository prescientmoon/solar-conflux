module Nihil.Parser.Module () where

import Data.Sequence ((|>))
import Data.Text qualified as Text
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Module
import Nihil.Parser.Core qualified as Core
import Optics qualified as O
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

{-
Module headers:

module A (exports) where
module A (exports)
module A where
module A
module (exports) where
module (exports)
module where
-}

module' ∷ Core.Parser Module
module' = M.label "module" $ Core.exactlyIndented do
  mbModule' ← M.optional $ Core.string "module"
  (name, exports, where') ← case mbModule' of
    Nothing → pure (Nothing, Nothing, Nothing)
    Just tmodule' → do
      name ← M.optional Core.name
      where' ← M.optional $ Core.string "where"
      pure (name, Nothing, where')
  eof ← Core.token M.eof
  pure $
    Module
      { module' = mbModule'
      , name = name
      , exports = exports
      , where' = where'
      , decls = []
      , eof = eof
      }
