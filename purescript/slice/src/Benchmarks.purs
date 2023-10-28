module Benchmarks where

import Prelude

import Benchotron.Core (Benchmark, benchFn, benchFn', mkBenchmark)
import Control.Monad.ST.Internal as ST
import Control.Monad.ST.Internal as STRef
import Data.Array (foldMap, foldr, foldl, (..))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.ST as STArray
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZipperArray
import Slice (Slice)
import Slice as Slice
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

listSum :: List Int -> Int
listSum (head:tail) = head + listSum tail
listSum _ = 0

listSumTCO :: List Int -> Int
listSumTCO = go 0
    where
    go s Nil = s
    go s (head:tail) = go (s + head) tail

zipperSum :: ZipperArray Int -> Int
zipperSum arr = ZipperArray.current arr + case ZipperArray.goNext arr of
    Nothing -> 0
    Just next -> zipperSum next

sliceSum :: Slice Int -> Int
sliceSum arr = case Slice.uncons arr of
    Nothing -> 0
    Just { head, tail } -> head + sliceSum tail

sliceSumTCO :: Slice Int -> Int
sliceSumTCO = go 0
    where 
    go s arr = case Slice.uncons arr of
        Nothing -> s
        Just { head, tail } -> go (s + head) tail

listFib :: Int -> List Int
listFib = go 1 1
    where
    go a b 0 = Nil
    go a b n = c:go c a (n - 1) 
        where
        c = a + b

stArrayFib :: Int -> Array Int 
stArrayFib to = STArray.run do
    a <- STRef.new 1
    b <- STRef.new 1
    result <- STArray.empty
    ST.for 1 to \_ -> do
        a' <- STRef.read a
        b' <- STRef.read b
        let c = a' + b'
        void 
            $ STArray.push c result
            <* STRef.write a' b
            <* STRef.write c a
    pure result

foreign import arraySum :: Array Int -> Int
foreign import arrayFib :: Int -> Array Int

benchSum :: Benchmark
benchSum = mkBenchmark
  { slug: "sum"
  , title: "Finding the sum of a sequence of integers"
  , sizes: (1..10) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> NonEmptyArray.cons' <$> arbitrary <*> vectorOf n arbitrary
  , functions: [ 
        -- benchFn' "array" arraySum NonEmptyArray.toArray
               benchFn' "list" listSum List.fromFoldable 
               , benchFn' "list (tail call optimization" listSumTCO List.fromFoldable
               , benchFn' "zipper" zipperSum ZipperArray.fromNonEmptyArray
               , benchFn' "array slice" sliceSum (NonEmptyArray.toArray >>> Slice.fromArray)
               , benchFn' "array slice (tail call optimization)" sliceSumTCO (NonEmptyArray.toArray >>> Slice.fromArray)
               , benchFn' "array foldr" (foldr (+) 0) NonEmptyArray.toArray
               , benchFn' "array foldl" (foldl (+) 0) NonEmptyArray.toArray
               , benchFn' "array foldMap" (foldMap Additive) NonEmptyArray.toArray
               , benchFn' "list foldr" (foldr (+) 0) List.fromFoldable
               , benchFn' "list foldl" (foldl (+) 0) List.fromFoldable
               , benchFn' "list foldMap" (foldMap Additive) List.fromFoldable
               ]
  }

benchFib :: Benchmark
benchFib = mkBenchmark
  { slug: "fibonacci"
  , title: "Generating the first n fibonacci numbers as a sequence"
  , sizes: (1..10) <#> (_ * 100)
  , sizeInterpretation: "Number of elements"
  , inputsPerSize: 1
  , gen: pure
  , functions: [ benchFn "array" arrayFib
               , benchFn "list" listFib
               , benchFn "stArray" stArrayFib
               ]
  }