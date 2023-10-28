module Sprint where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Type.Data.List (type (:>), List', Nil')
import Unsafe.Coerce (unsafeCoerce)

data State s a
    = Get (s -> a)
    | Put s (Unit -> a)

type Ability = Type -> Type

data Sprint :: List' Ability -> Type -> Type
data Sprint effects a 
    = Pure a
    | Bind (forall t. (forall eff. Functor eff => String -> WithContinuation eff effects a -> t) -> t)

type WithContinuation :: Ability -> List' Ability -> Type -> Type
type WithContinuation eff effects a = eff (Sprint effects a)

flat :: forall a effects. Sprint effects (Sprint effects a) -> Sprint effects a
flat = case _ of
    Pure m -> m
    Bind f -> f \n eff -> Bind \cont -> cont n $ map flat eff

handle :: forall rest eff a t. String -> Sprint (eff :> rest) a -> (a -> t) -> (WithContinuation eff (eff :> rest) a -> t) -> Sprint rest t
handle name m casePure caseBind = case m of
    Pure a -> Pure $ casePure a
    Bind f -> f go 
        where 
        go ::  forall eff'. Functor eff' => String -> WithContinuation eff' (eff :> rest) a -> Sprint rest t
        go name' eff = case name == name' of
            false -> Bind \f' -> f' name' $ map (\m' -> handle name m' casePure caseBind) eff
            true -> Pure $ caseBind (unsafeCoerce eff)

evalState :: forall rest s a. s -> Sprint (State s :> rest) a -> Sprint rest a
evalState s m = flat $ handle "State" m Pure case _ of
    Put s' continue -> evalState s' $ continue unit 
    Get continue -> evalState s $ continue s

extract :: forall a. Sprint Nil' a -> a
extract = case _ of
    Pure a -> a
    _ -> unsafeCrashWith "can't extract :("

---------- Typeclass instances
derive instance functorState :: Functor (State s)
