module Moonlog.Expression where


import Prelude

import Data.Array (foldMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Lens (Lens, over)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Nullable
import Data.String (joinWith)
import Data.Traversable (for, traverse)
import Data.Variant (match)
import Moonlog.Parser.Cst (class HasSpan, sourceSpan, withoutSpan)
import Moonlog.Parser.Cst as Cst
import Run (Run)
import Run.State (STATE, evalState, get, modify)
import Run.Supply (SUPPLY, generate, localSupply)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type Constructor a = { span :: Cst.SourceSpan, name :: String, arguments :: Array a }

data Ast var
    = Var Cst.SourceSpan var
    | Constructor (Constructor (Ast var))

type Generalized = Ast Int
type Expression = Ast String

type Rule a =
    { head :: Constructor a
    , body :: Array (Constructor a) }

--------- Constructors
ruleFromCst :: Cst.Rule -> Rule Expression
ruleFromCst { head, body } = { head: constructorFromCst head, body: constructorFromCst <$> body }

constructorFromCst :: Cst.Pattern -> Constructor Expression
constructorFromCst pattern@(Cst.Pattern { arguments, name }) =
    { span: sourceSpan pattern
    , arguments: fromCst <$> arguments
    , name: withoutSpan name }

fromCst :: Cst.Term -> Expression
fromCst (Cst.Term t) = t # match
    { var: \s -> Var (sourceSpan s) (withoutSpan s)
    , pattern: \c -> Constructor $ constructorFromCst c
    , natural: fromNatural
    , list: fromList  }
    where
    fromList :: Cst.List -> Expression
    fromList { span, elements, tail } = go (List.fromFoldable elements) 
        where
        go List.Nil = maybe nil fromCst $ Nullable.toMaybe tail
        go (List.Cons head rest) = cons (fromCst head) $ go rest

        nil :: Expression
        nil = Constructor { span, name: "Nil", arguments: [] }

        cons :: Expression -> Expression -> Expression
        cons head rest = Constructor { span, name: "Cons", arguments: [head, rest] }

    fromNatural :: Cst.WithSpan Int -> Expression
    fromNatural (Cst.WithSpan { value, span }) = go value
        where
        go a | a > 0 = succ $ go (a - 1)
             | otherwise = zero

        zero :: Expression
        zero = Constructor { span, name: "Z", arguments: [] }

        succ :: Expression -> Expression
        succ previous = Constructor { span, name: "S", arguments: [previous]  }


---------- Helpers
-- TODO: remove the boilerplate from these 2
-- | Alias for the var cache used in insantiation
type GENERALIZE r = STATE (HashMap String Int) r

-- | Helper to run the instantiation process
runGeneralization :: forall r a. Run (GENERALIZE r) a -> Run r a
runGeneralization = evalState HashMap.empty

generalize :: forall r. Expression -> Run (GENERALIZE r) Generalized
generalize = go 
    where
    go (Var span var) = do
        s <- get
        id <- case HashMap.lookup var s of
            Just id -> pure id
            Nothing -> do
                let id = HashMap.size s
                modify (HashMap.insert var id)
                pure id
        pure $ Var span id
    go (Constructor original) = ado
        arguments <- traverse go original.arguments
        in Constructor { span: original.span, name: original.name, arguments }

generalizeConstructor :: forall r. Constructor Expression -> Run (GENERALIZE r) (Constructor Generalized)
generalizeConstructor constructor = do
    arguments <- for constructor.arguments generalize
    pure $ constructor { arguments = arguments }

generalizeRule :: forall r. Rule Expression -> Run (GENERALIZE r) (Rule Generalized)
generalizeRule rule = ado
    head <- generalizeConstructor rule.head
    body <- for rule.body generalizeConstructor
    in { head, body }

-- | Alias for the var cache used in insantiation
type INSTANTIATE r = STATE (HashMap Int String) r

-- | Helper to run the instantiation process
runInstantiation :: forall r a. Run (INSTANTIATE r) a -> Run r a
runInstantiation = evalState HashMap.empty

instantiate :: forall r. Generalized -> Run (SUPPLY Int + INSTANTIATE r) Expression
instantiate = go >>> localSupply (show >>> (<>) "#")
    where
    go (Var span var) = do
        s <- get
        name <- case HashMap.lookup var s of
            Just name -> pure name
            Nothing -> do
                name <- generate
                modify (HashMap.insert var name)
                pure name
        pure $ Var span name
    go (Constructor original) = ado
        arguments <- traverse go original.arguments
        in Constructor { span: original.span, name: original.name, arguments }

instantiateConstructor :: forall r. Constructor Generalized -> Run (SUPPLY Int + INSTANTIATE r) (Constructor Expression)
instantiateConstructor constructor = do
    arguments <- for constructor.arguments instantiate
    pure $ constructor { arguments = arguments }

instantiateRule :: forall r. Rule Generalized -> Run (SUPPLY Int + INSTANTIATE r) (Rule Expression)
instantiateRule rule = ado
    head <- instantiateConstructor rule.head
    body <- for rule.body instantiateConstructor
    in { head, body }

-- | Like map but for rules :D
mapRule :: forall a b. (a -> b) -> Rule a -> Rule b
mapRule f { head, body } = { head: mapPattern f head, body: map (mapPattern f) body }

-- | Like map but for patterns
mapPattern :: forall a b. (a -> b) -> Constructor a -> Constructor b
mapPattern f= over _arguments (map f)

-- | Get all free vars inside an expression
freeVars :: forall a. Hashable a => Ast a -> HashSet.HashSet a
freeVars (Var _ a) = HashSet.singleton a
freeVars (Constructor { arguments }) = foldMap freeVars arguments

---------- Typeclass instances
instance hasSpanAst :: HasSpan (Ast v) where
    sourceSpan (Var s v) = s
    sourceSpan (Constructor c) = c.span

instance showAst :: Show (Ast String) where
    show (Var _ v) = v
    show (Constructor c) = c.name <> " " <> joinWith " " (withParens <$> c.arguments)
        where
        withParens (Var _ v) = v
        withParens c'@(Constructor _) = "(" <> show c' <> ")"

---------- Lenses
_arguments :: forall a b. Lens (Constructor a) (Constructor b) (Array a) (Array b)
_arguments = prop (Proxy :: _ "arguments")