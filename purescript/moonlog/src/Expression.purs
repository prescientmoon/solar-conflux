module Moonlog.Expression where


import Prelude

import Data.HashMap as HashMap
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Nullable
import Data.Traversable (traverse)
import Data.Variant (match)
import Moonlog.Parser.Cst (sourceSpan, withoutSpan)
import Moonlog.Parser.Cst as Cst
import Run (Run, extract)
import Run.State (evalState, get, modify)
import Run.Supply (SUPPLY, generate, localSupply)
import Undefined (undefined)

type Constructor a = { span :: Cst.SourceSpan, name :: String, arguments :: Array a }

data Ast var
    = Var Cst.SourceSpan var
    | Constructor (Constructor (Ast var))

type Generalized = Ast Int
type Expression = Ast String

--------- Constructors
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
    , list: \_ -> someAst }
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

    someAst :: Expression
    someAst = undefined

---------- Helpers
-- TODO: remove the boilerplate from these 2
generalize :: Expression -> Generalized
generalize = go >>> evalState HashMap.empty >>> extract
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

instantiate :: forall r. Generalized -> Run (SUPPLY Int r) Expression
instantiate = go >>> evalState HashMap.empty >>> localSupply (show >>> (<>) "_V")
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


---------- Typeclass instances