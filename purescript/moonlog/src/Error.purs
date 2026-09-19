module Moonlog.Error where

import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Moonlog.Parser.Cst (SourceSpan)
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as EXCEPT
import Prelude

type WITH_ERRORS :: forall k. Row Type -> Row (k -> Type) -> Row (k -> Type)
type WITH_ERRORS e r = EXCEPT (Variant e /\ SourceSpan) r

throw :: forall e a r. SourceSpan -> Variant e -> Run (WITH_ERRORS e r) a
throw = flip $ curry EXCEPT.throw