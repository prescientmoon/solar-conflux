module Sky.Language.Cst where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (foldr, for_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Tuple (swap)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Run (Run)
import Run as Run
import Run.State (STATE)
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Sky.Language.Ast (Ast)
import Sky.Language.Ast as Ast
import Sky.Language.Error (class SourceSpot)
import Sky.Language.Term (Name(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Base types
newtype SourcePosition = SourcePosition { line :: Int, column :: Int, index :: Int }
newtype SourceSpan = SourceSpan { start :: SourcePosition, end :: SourcePosition }
type WithSpan a =
  { value :: a
  , span :: SourceSpan
  }

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
type SourceMap = HashMap AbstractSourceLocation SourceSpan

-- | Base monad containing all required effects for converting from CST to AST
type CstConversionM r = Run (STATE SourceMap + SUPPLY Int r)

-- | Rememebr the source span associated with a location
rememberSpan :: forall r. AbstractSourceLocation -> SourceSpan -> CstConversionM r Unit
rememberSpan key span = State.modify $ HM.insert key span

-- | Run a computation in the cst conversion monad
runCstM :: forall a. CstConversionM () a -> a /\ SourceMap
runCstM = Supply.runSupply ((+) 1) 0 >>> State.runState HM.empty >>> Run.extract >>> swap

toplevelScopeToAst :: TopLevelScope -> Ast AbstractSourceLocation /\ SourceMap
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

data AbstractSourceLocation
  = LambdaArgument AbstractSourceLocation
  | PiArgument AbstractSourceLocation
  | NameOfLet AbstractSourceLocation
  | TermLocation RawLocationId
  | Nowhere

-- | Convert from cst to ast
indexSourcePositions
  :: forall r
   . Cst
  -> CstConversionM r (Ast AbstractSourceLocation)
indexSourcePositions (Cst { value, span }) = do
  id <- Supply.generate <#> (RawLocationId >>> TermLocation)
  rememberSpan id span
  Variant.match
    { hole: \{ name } -> pure $ Ast.EHole id $ Name <$> Nullable.toMaybe name
    , star: \{} -> pure $ Ast.EStar id
    , var: \{ name } -> pure $ Ast.EVar id $ Name name
    , lambda: \{ argument, body } -> ado
        rememberSpan (LambdaArgument id) (extractSpan argument)
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
          rememberSpan (PiArgument id) (extractSpan argument)
        let name' = Name $ maybe "_" _.value $ Nullable.toMaybe name
        in Ast.EPi id name' domain codomain
    , "let": \{ definition: { name, value }, body } -> ado
        value <- indexSourcePositions value
        body <- indexSourcePositions body
        rememberSpan (NameOfLet id) (extractSpan name)
        in Ast.ELet id (Name name.value) value body
    , assumption: \{ type: type_ } -> Ast.EAssumption id <$> indexSourcePositions type_
    }
    value

---------- Typeclass isntances
derive newtype instance Eq RawLocationId
derive newtype instance Hashable RawLocationId

derive instance Newtype SourceSpan _
derive instance Newtype SourcePosition _
derive instance Newtype Cst _

derive instance Eq SourceSpan
derive instance Eq SourcePosition

instance Ord SourcePosition where
  compare = on compare (Newtype.unwrap >>> _.index)

instance Semigroup SourceSpan where
  append (SourceSpan a) (SourceSpan b) = SourceSpan
    { start: min a.start b.start
    , end: max a.end b.end
    }

derive newtype instance Show RawLocationId
derive instance Eq AbstractSourceLocation
derive instance Generic AbstractSourceLocation _

instance Show AbstractSourceLocation where
  show a = genericShow a

instance Hashable AbstractSourceLocation where
  hash a = a # morph # hash
    where
    morph
      :: AbstractSourceLocation
      -> Either (Either AbstractSourceLocation (Either AbstractSourceLocation AbstractSourceLocation)) (Maybe RawLocationId)
    morph (LambdaArgument a) = Left $ Left a
    morph (NameOfLet a) = Left $ Right $ Left a
    morph (PiArgument a) = Left $ Right $ Right a
    morph (TermLocation a) = Right $ Just a
    morph Nowhere = Right Nothing

instance SourceSpot AbstractSourceLocation where
  nowhere = Nowhere
  nameOfLet = NameOfLet
  piArgument = PiArgument
  lambdaArgument = LambdaArgument

---------- Proxies
_let :: Proxy "let"
_let = Proxy

_var :: Proxy "var"
_var = Proxy