module Naturals where

import Prelude

import Type.Proxy (Proxy(..))

class IsNatural :: Natural -> Constraint
class IsNatural a where
    match :: Proxy a 
        -> forall r. (forall inner. IsNatural inner => Proxy inner -> r) 
        -> (forall zero. IsNatural zero => Proxy zero -> r) 
        -> r

foreign import data Natural :: Type
foreign import data Succ :: forall k. k -> Natural
foreign import data Zero :: Natural

instance isNaturalZero :: IsNatural Zero where
    match _ caseSucc caseZero = caseZero (Proxy :: Proxy Zero)

instance isNaturalSucc :: IsNatural a => IsNatural (Succ a) where
    match _ caseSucc caseZero = caseSucc (Proxy :: Proxy a)

foreign import data Add :: forall k. k -> k -> Natural

instance isNaturalAdd :: (IsNatural a, IsNatural b) => IsNatural (Add a b) where
    match _ caseSucc caseZero = match (Proxy :: _ a)     
        handleFirstSucc
        handleFirstZero
        where
        handleFirstZero _ = match (Proxy :: _ b) caseSucc caseZero
    
        handleFirstSucc :: forall previous. IsNatural previous => Proxy previous -> _
        handleFirstSucc _ = match (Proxy :: _ (Succ (Add previous b))) caseSucc caseZero

reflectNat :: forall n. IsNatural n => Proxy n -> Int
reflectNat p = match p (\inner -> 1 + reflectNat inner) (const 0)

{-
--------- Equality
foreign import data Equality :: forall k. k -> k -> Type
foreign import data Refl :: forall a. Equality a a

unsafeRefl :: forall a b. Equality a b
unsafeRefl = undefined

mapEquality :: forall a b f. Proxy f -> Equality a b -> Equality (f a) (f b)
mapEquality _ _ = unsafeRefl

zeroPlusAIsA :: forall a. Proxy a -> Equality (Add Zero a) a
zeroPlusAIsA _ = unsafeRefl 

succRule :: forall a b. Equality (Add (Succ a) b) (Succ (Add a b))
succRule = unsafeRefl

aPlusZeroIsA :: forall a. IsNatural a => Proxy a -> Equality (Add a Zero) a 
aPlusZeroIsA p = match p 
    caseSucc
    caseZero
    where
    caseZero p = zeroPlusAIsA p 
    caseSucc inner = mapEquality (Proxy :: _ Succ) (aPlusZeroIsA inner)

type ZeroIsZero :: Equality Zero Zero
type ZeroIsZero = Refl

data Natural id
    = Zero (TZero ~ id)
    | Succ (forall r. (forall inner. Natural inner -> (Succ inner ~ id) -> r) -> r)

zeroPlusAIsA :: forall id. Proxy id -> Equality id (Add Zero id)
zeroPlusAIsA = ...

succEquality :: forall a b. Proxy a -> Proxy b -> Equality (Succ (Add a b)) (Add (Succ a) b)
succEquality = ...

aPlusZeroIsA :: forall r id. Natural id -> Equality id (Add id Zero)
aPlusZeroIsA (Zero proof) 
    = zeroPlusAIsA @Zero -- Equality Zero (Add Zero Zero)
    |> rewriteLeft proof -- Equality id (Add Zero Zero)
    |> rewriteRight 
        (additionEquality proof identity) -- Equlity (Add id Zero) (Add Zero Zero)
            -- Equality id (Add id Zero)
    
aPlusZeroIsA (Succ existential) = existential handleExistential
    where
    handleExistential :: forall inner. Natural inner -> (Succ inner ~ id) -> Equality id (Add id Zero)
    handleExistential inner proof
        = aPlusZeroIsA inner -- Equality inner (Add inner Zero)
        |> rewriteWithContext (Proxy :: _ Succ) -- Equality (Succ inner) (Succ (Add inner Zero))
        |> rewriteLeft proof -- Equality id (Succ (Add inner Zero))
        |> rewriteRight 
            (succEquality @inner @Zero) -- Equality (Succ (Add inner Zero)) (Add (Succ inner) Zero)
                -- Equality id (Add (Succ inner) Zero)
        |> rewriteRight 
            (additionEquality proof identity) -- Equality (Add (Succ inner) Zero) (Add id Zero)
                -- Equality id (Add id Zero)


-}

---------- Helpers
type One = Succ Zero
type Two = Succ One
type Three = Succ Two

one :: Proxy One 
one = Proxy 

two :: Proxy Two
two = Proxy 

three :: Proxy Three
three = Proxy 

add :: forall a b. Proxy a -> Proxy b -> Proxy (Add a b)
add _ _ = Proxy