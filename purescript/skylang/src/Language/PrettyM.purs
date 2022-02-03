module Sky.Language.PrettyM where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Record as Record
import Run (Run)
import Run as Run
import Run.Reader (Reader)
import Run.Reader as Reader
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Safe.Coerce (coerce)
import Sky.Language.Error (class SourceSpot, indexOriginalBuffer)
import Sky.Language.MetaVar (META_CONTEXT, lookupMetaName)
import Sky.Language.Source (SourceMap)
import Sky.Language.Term (MetaContext, MetaVar, Name(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Effect stuff
newtype PrintContext a = PrintContext
  { scope :: Array String
  , sourceMap :: SourceMap a
  , originalText :: String
  }

-- | The context necessary for pretty printing to take place
type PRINT_CONTEXT a r = (printContext :: Reader (PrintContext a) | r)
-- | Monad carrying a print context
type PrintM a r = Run (SUPPLY Int + PRINT_CONTEXT a + META_CONTEXT a r)

-- | Expose the current scope
getScope :: forall a r. PrintM a r (Array String)
getScope = Reader.askAt _printContext <#> \(PrintContext { scope }) -> scope

-- | Expose the current source map
getSourceMap :: forall a r. PrintM a r (SourceMap a)
getSourceMap = Reader.askAt _printContext <#> \(PrintContext { sourceMap }) -> sourceMap

-- | Expose the text which got parsed into the ast we are working on 
getOriginalText :: forall a r. Run (PRINT_CONTEXT a r) String
getOriginalText = Reader.askAt _printContext <#> \(PrintContext { originalText }) -> originalText

-- | Add a bunch of variables in scope 
extendPrintScope :: forall a r. Array String -> PrintM a r ~> PrintM a r
extendPrintScope with = Reader.localAt _printContext $ Newtype.over PrintContext
  $ Record.modify _scope \a -> with <> a

generateVar :: forall a r. PrintM a r String
generateVar = ado
  id <- Supply.generate
  in "_" <> show id

-- | Lookup a meta name, and if it doesn't exist, generate a new one
generateMetaName :: forall a r. MetaVar -> PrintM a r String
generateMetaName = lookupMetaName >=> maybe generateVar (coerce >>> pure)

lookupSource :: forall a r. SourceSpot a => a -> PrintM a r String
lookupSource source = do
  sourceMap <- getSourceMap
  originalText <- getOriginalText
  case indexOriginalBuffer originalText sourceMap source of
    Nothing -> generateVar
    Just name -> pure name

-- | Unwrap a computations running in the printM monad
runPrintM :: forall o a. MetaContext a -> PrintContext a -> PrintM a () o -> o
runPrintM metas ctx = Reader.runReaderAt _printContext ctx
  >>> Supply.runSupply ((+) 1) 0
  >>> State.evalStateAt _metaContext metas
  >>> Run.extract

-------- Typeclass instances
derive instance Newtype (PrintContext a) _

---------- Proxies
_scope :: Proxy "scope"
_scope = Proxy

_metaContext :: Proxy "metaContext"
_metaContext = Proxy

_printContext :: Proxy "printContext"
_printContext = Proxy