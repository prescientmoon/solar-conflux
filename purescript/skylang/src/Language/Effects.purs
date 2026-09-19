module Sky.Language.Effects
  ( ActionId(..)
  , ELABORATION_CONTEXT
  , EVALUATION_ENV
  , ElabM
  , ElaborationContext(..)
  , EvalM
  , PRINT_CONTEXT
  , PrintContext(..)
  , PrintM
  , QUOTATION_ENV
  , QuoteEnv(..)
  , SKY_EFFECTS
  , SKY_LOGGER
  , SkyAction(..)
  , SkyLog(..)
  , SkyM
  , _elaborationContext
  , _evaluationEnv
  , _metaContext
  , _printContext
  , _quotationEnv
  , _scope
  , _skyLogger
  , augumentElaborationContext
  , augumentEnv
  , environment
  , extendPrintScope
  , generateMetaName
  , getDepth
  , generateVar
  , getElaborationContext
  , getOriginalText
  , getScope
  , getSourceMap
  , increaseDepth
  , lookupSource
  , runElaborationContext
  , runEvaluationEnv
  , runPrintM
  , runQuotationEnv
  , setPrintScope
  ) where

import Prelude

import Ansi.Codes (GraphicsParam)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Tuple.Nested (type (/\))
import Dodo (Doc)
import Record as Record
import Run (Run)
import Run as Run
import Run.Reader (Reader)
import Run.Reader as Reader
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Run.Writer (Writer)
import Safe.Coerce (coerce)
import Sky.Language.Error (class SourceSpot, SKY_ERROR, indexOriginalBuffer)
import Sky.Language.MetaVar (META_CONTEXT, lookupMetaName)
import Sky.Language.Source (SourceMap)
import Sky.Language.Term (Env, Level(..), Mask, MetaContext, MetaVar, Name(..), NameEnv(..), Value)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- The general stack
-- | Set of effects that *should* be used everywhere
type SKY_EFFECTS a r =
  META_CONTEXT a
    + SKY_ERROR a
    + SUPPLY Int
    + QUOTATION_ENV
    + SKY_LOGGER
    + PRINT_CONTEXT a r

-- | Monad carrying the set of effects that should be carried around everywehre
type SkyM a r = Run (SKY_EFFECTS a r)

---------- Logging
newtype ActionId = ActionId Int

data SkyAction d
  = Checking (Doc d) (Doc d)
  | Inferring (Doc d)
  | Unifying (Doc d) (Doc d)
  | Evaluating (Doc d)

data SkyLog
  = Started ActionId (SkyAction GraphicsParam)
  | Finished ActionId (Doc GraphicsParam)

type SKY_LOGGER r = (skyLogger :: Writer (Array SkyLog) | r)

---------- Elaboration
-- | Context required by elaboration
newtype ElaborationContext a = ElaborationContext
  { types :: HashMap Name (Level /\ Value a)
  , mask :: Mask
  }

-- | Context required for elaboration to take place in
type ELABORATION_CONTEXT a r =
  ( elaborationContext :: Reader (ElaborationContext a)
  | r
  )

-- | Base monad containing all the effects required for evaluation to compute
type ElabM a r = Run
  ( ELABORATION_CONTEXT a + META_CONTEXT a
      + SKY_ERROR a
      + QUOTATION_ENV
      + EVALUATION_ENV a
      + SUPPLY Int
      + PRINT_CONTEXT a
      + SKY_LOGGER r
  )

-- | Run a computation which makes use of an elaboration context. 
runElaborationContext :: forall a r. Run (ELABORATION_CONTEXT a r) ~> Run r
runElaborationContext = Reader.runReaderAt _elaborationContext $ ElaborationContext
  { mask: mempty
  , types: HM.empty
  }

-- | Expose the current elaboration context
getElaborationContext :: forall a r. Run (ELABORATION_CONTEXT a r) (ElaborationContext a)
getElaborationContext = Reader.askAt _elaborationContext

-- | Run a computation in a modified version of the current context
augumentElaborationContext
  :: forall a r
   . (ElaborationContext a -> ElaborationContext a)
  -> Run (ELABORATION_CONTEXT a r) ~> Run (ELABORATION_CONTEXT a r)
augumentElaborationContext = Reader.localAt _elaborationContext

---------- Evaluation & Quoting
newtype QuoteEnv = QuoteEnv { depth :: Level }

type EVALUATION_ENV a r = (evaluationEnv :: Reader (Env a) | r)
type QUOTATION_ENV r = (quotationEnv :: Reader QuoteEnv | r)

-- | Base monad with everything required for evaluation to take place
type EvalM a r = SkyM a (EVALUATION_ENV a r)

-- | Expose the current evaluation environment
environment :: forall a r. Run (EVALUATION_ENV a r) (Env a)
environment = Reader.askAt _evaluationEnv

-- | Expose the current depth from the context
getDepth :: forall r. Run (QUOTATION_ENV r) Level
getDepth = Reader.askAt _quotationEnv
  <#> \(QuoteEnv { depth }) -> depth

-- | Run a computation in a context deeper by 1 than the current one
increaseDepth :: forall r. Run (QUOTATION_ENV r) ~> Run (QUOTATION_ENV r)
increaseDepth = Reader.localAt _quotationEnv
  \(QuoteEnv { depth }) -> QuoteEnv { depth: coerce ((+) 1) depth }

-- | Run a computation in a modified environment
augumentEnv :: forall a r. (Env a -> Env a) -> Run (EVALUATION_ENV a r) ~> Run (EVALUATION_ENV a r)
augumentEnv = Reader.localAt _evaluationEnv

-- | Run a computation which makes use of the evaluation env
runEvaluationEnv :: forall a r. Run (EVALUATION_ENV a r) ~> Run r
runEvaluationEnv = Reader.runReaderAt _evaluationEnv mempty

-- | Run a computation which makes use of the quotation env
runQuotationEnv :: forall r. Run (QUOTATION_ENV r) ~> Run r
runQuotationEnv = Reader.runReaderAt _quotationEnv $ QuoteEnv { depth: coerce 0 }

---------- Pretty printing 
newtype PrintContext a = PrintContext
  { scope :: NameEnv
  , sourceMap :: SourceMap a
  , originalText :: String
  }

-- | The context necessary for pretty printing to take place
type PRINT_CONTEXT a r = (printContext :: Reader (PrintContext a) | r)
-- | Monad carrying a print context
type PrintM a r = Run (SUPPLY Int + PRINT_CONTEXT a + META_CONTEXT a r)

-- | Expose the current scope
getScope :: forall a r. Run (PRINT_CONTEXT a r) NameEnv
getScope = Reader.askAt _printContext <#> \(PrintContext { scope }) -> scope

-- | Expose the current source map
getSourceMap :: forall a r. Run (PRINT_CONTEXT a r) (SourceMap a)
getSourceMap = Reader.askAt _printContext <#> \(PrintContext { sourceMap }) -> sourceMap

-- | Expose the text which got parsed into the ast we are working on 
getOriginalText :: forall a r. Run (PRINT_CONTEXT a r) String
getOriginalText = Reader.askAt _printContext <#> \(PrintContext { originalText }) -> originalText

-- | Add a bunch of variables in scope 
extendPrintScope :: forall a r. Array String -> PrintM a r ~> PrintM a r
extendPrintScope with = Reader.localAt _printContext $ Newtype.over PrintContext
  $ Record.modify _scope \a -> NameEnv with <> a

-- | Completly overwrite the current scope
setPrintScope :: forall a r. NameEnv -> PrintM a r ~> PrintM a r
setPrintScope to = Reader.localAt _printContext $ Newtype.over PrintContext
  $ Record.set _scope to

generateVar :: forall a r. Run (PRINT_CONTEXT a + SUPPLY Int r) String
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

---------- Typeclass instances
derive instance Eq ActionId
derive newtype instance Show ActionId
derive instance Newtype (PrintContext a) _

---------- Proxies
_elaborationContext :: Proxy "elaborationContext"
_elaborationContext = Proxy

_evaluationEnv :: Proxy "evaluationEnv"
_evaluationEnv = Proxy

_quotationEnv :: Proxy "quotationEnv"
_quotationEnv = Proxy

_scope :: Proxy "scope"
_scope = Proxy

_metaContext :: Proxy "metaContext"
_metaContext = Proxy

_printContext :: Proxy "printContext"
_printContext = Proxy

_skyLogger :: Proxy "skyLogger"
_skyLogger = Proxy