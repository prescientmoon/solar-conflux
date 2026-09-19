module Sky.Language.Cst where

import Prelude
import Sky.Language.Source (SourceSpan, SourceMap, WithSpan)

import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Debug (class Debug, opaque_)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (foldr, for_)
import Data.Generic.Rep (class Generic)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (swap)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Record as Record
import Run (Run)
import Run as Run
import Run.State (STATE)
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Safe.Coerce (coerce)
import Sky.Language.Ast (Ast)
import Sky.Language.Ast as Ast
import Sky.Language.Error (class SourceSpot, lambdaArgument, nameOfLet, piArgument)
import Sky.Language.Term (Name(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Cst types
type TopLevelScope = NonEmptyArray
  { name :: WithSpan String
  , value :: Cst
  }

type RawCst = Variant
  ( star :: {}
  , hole ::
      { name :: Nullable String

      }
  , var :: { name :: String }
  , lambda ::
      { argument :: WithSpan String
      , body :: Cst

      }
  , application ::
      { function :: Cst
      , argument :: Cst
      }
  , annotation ::
      { value :: Cst
      , type :: Cst
      }
  , pi ::
      { name :: Nullable (WithSpan String)
      , domain :: Cst
      , codomain :: Cst
      }
  , assumption ::
      { type :: Cst
      }
  , "let" ::
      { definition ::
          { name :: WithSpan String
          , value :: Cst
          }
      , body :: Cst
      }
  )

-- | The result of parsing a term on the js side
newtype Cst =
  Cst (WithSpan RawCst)

---------- Helpers
-- | Extra the spam from a value which contains one
extractSpan :: forall a. WithSpan a -> SourceSpan
extractSpan = _.span

---------- Effect related stuff
-- | Not an actual source map, but encapsulates the same idea

-- | Base monad containing all required effects for converting from CST to AST
type CstConversionM r = Run (STATE (SourceMap AbstractSourceLocation) + SUPPLY Int r)

-- | Rememebr the source span associated with a location
rememberSpan :: forall r. AbstractSourceLocation -> SourceSpan -> CstConversionM r Unit
rememberSpan key span = State.modify $ HM.insert key span

-- | Run a computation in the cst conversion monad
runCstM :: forall a. CstConversionM () a -> a /\ SourceMap AbstractSourceLocation
runCstM = Supply.runSupply ((+) 1) 0 >>> State.runState HM.empty >>> Run.extract >>> swap

toplevelScopeToAst :: TopLevelScope -> Ast AbstractSourceLocation /\ SourceMap AbstractSourceLocation
toplevelScopeToAst scope = scope
  # foldr
      createLet
      lastInScope
  # indexSourcePositions
  # runCstM
  where
  lastInScope :: Cst
  lastInScope = NonEmptyArray.last scope #
    \{ name } -> Cst { span: name.span, value: Variant.inj _var { name: name.value } }

  createLet :: { value :: Cst, name :: WithSpan String } -> Cst -> Cst
  createLet { name, value } body = Cst
    { span: name.span <> (Newtype.unwrap value).span
    , value:
        ( Variant.inj _let
            { definition:
                { name
                , value
                }
            , body
            } :: RawCst
        )
    }

---------- Source positions
newtype RawLocationId = RawLocationId Int

data LocationKind
  = LambdaArgument
  | PiArgument
  | NameOfLet
  | TermLocation

newtype AbstractSourceLocation = AbstractSourceLocation
  ( Maybe
      { raw :: RawLocationId
      , locationKind :: LocationKind
      }
  )

-- | Convert from cst to ast
indexSourcePositions
  :: forall r
   . Cst
  -> CstConversionM r (Ast AbstractSourceLocation)
indexSourcePositions (Cst { value, span }) = do
  id <- Supply.generate <#> (RawLocationId >>> { raw: _, locationKind: TermLocation } >>> Just >>> AbstractSourceLocation)
  rememberSpan id span
  Variant.match
    { hole: \{ name } -> pure $ Ast.EHole id $ Name <$> Nullable.toMaybe name
    , star: \{} -> pure $ Ast.EStar id
    , var: \{ name } -> pure $ Ast.EVar id $ Name name
    , lambda: \{ argument, body } -> ado
        rememberSpan (lambdaArgument id) (extractSpan argument)
        body <- indexSourcePositions body
        in Ast.ELambda id (Name argument.value) body
    , application: \{ function, argument } -> ado
        function <- indexSourcePositions function
        argument <- indexSourcePositions argument
        in Ast.EApplication id function argument
    , annotation: \{ type: type_, value } -> ado
        type_ <- indexSourcePositions type_
        value <- indexSourcePositions value
        in Ast.EAnnotation id value type_
    , pi: \{ name, domain, codomain } -> ado
        domain <- indexSourcePositions domain
        codomain <- indexSourcePositions codomain
        for_ (Nullable.toMaybe name) \argument ->
          rememberSpan (piArgument id) (extractSpan argument)
        let name' = Name $ maybe "_" _.value $ Nullable.toMaybe name
        in Ast.EPi id name' domain codomain
    , "let": \{ definition: { name, value }, body } -> ado
        value <- indexSourcePositions value
        body <- indexSourcePositions body
        rememberSpan (nameOfLet id) (extractSpan name)
        in Ast.ELet id (Name name.value) value body
    , assumption: \{ type: type_ } -> Ast.EAssumption id <$> indexSourcePositions type_
    }
    value

---------- Typeclass isntances
derive newtype instance Eq RawLocationId
derive newtype instance Hashable RawLocationId

derive instance Newtype Cst _
derive instance Newtype AbstractSourceLocation _

derive instance Eq LocationKind
derive instance Ord LocationKind
derive instance Generic LocationKind _

derive newtype instance Show RawLocationId
derive instance Eq AbstractSourceLocation
derive instance Generic AbstractSourceLocation _

instance Show AbstractSourceLocation where
  show a = "---"

instance Debug AbstractSourceLocation where
  debug _ = opaque_ "location"

instance Hashable AbstractSourceLocation where
  hash (AbstractSourceLocation Nothing) = 0
  hash (AbstractSourceLocation (Just { raw, locationKind })) = 1 + (coerce raw * coerce count) + fromEnum locationKind
    where
    count :: Cardinality LocationKind
    count = cardinality

instance Enum LocationKind where
  succ = genericSucc
  pred = genericPred

instance Bounded LocationKind where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum LocationKind where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance SourceSpot AbstractSourceLocation where
  nowhere = AbstractSourceLocation Nothing
  nameOfLet = Newtype.over AbstractSourceLocation $ map $ Record.set _locationKind NameOfLet
  piArgument = Newtype.over AbstractSourceLocation $ map $ Record.set _locationKind PiArgument
  lambdaArgument = Newtype.over AbstractSourceLocation $ map $ Record.set _locationKind LambdaArgument

---------- Proxies
_let :: Proxy "let"
_let = Proxy

_var :: Proxy "var"
_var = Proxy

_locationKind :: Proxy "locationKind"
_locationKind = Proxy