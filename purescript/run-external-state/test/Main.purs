module Test.Main where

import Prelude

import Data.Int (floor, toNumber)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Run (extract, runBaseEffect)
import Run.State.External (get, imapExternaState, put, runExternalStatePure, runExternalStateUsingRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

-- TODO: finish writing tests
spec :: Spec Unit
spec = describe "External state" do
  describe "runExternalStatePure" do
    it "should return the initial state while doing nothing" do
      let num = 103
      let val = fst $ extract $ runExternalStatePure num (pure unit)
      val `shouldEqual` num
    it "should allow getting the current value" do
      let initial = 96
      let val = snd $ extract $ runExternalStatePure initial get
      val `shouldEqual` initial
    it "should allow setting the current state" do
      let initial = 48
      let other = 32
      let final = fst $ extract $ runExternalStatePure initial $ put other
      final `shouldEqual` other
  describe "imapExternalState" do
    it "should allow mapping over the state" do
      let initial = 13.0
      let other = 20
      let computation = imapExternaState floor toNumber do
           state <- get
           put other
           pure (state * 2)
      let final = extract $ runExternalStatePure initial computation
      final `shouldEqual` (20.0 /\ 26)
  describe "runExteranlStateUsingRef" do
    it "should not modify the value while doing nothing" do
      let initial = 10
      ref <- liftEffect $ Ref.new initial
      liftEffect $ runBaseEffect $ runExternalStateUsingRef ref (pure unit)
      current <- liftEffect $ Ref.read ref
      current `shouldEqual` initial
    it "should allow setting the value" do
      let initial = 49
      let other = 13
      ref <- liftEffect $ Ref.new initial
      liftEffect $ runBaseEffect $ runExternalStateUsingRef ref (put other)
      current <- liftEffect $ Ref.read ref
      current `shouldEqual` other
    it "should allow getting the current value" do
      let initial = 78
      ref <- liftEffect $ Ref.new initial
      result <- liftEffect $ runBaseEffect $ runExternalStateUsingRef ref get
      result `shouldEqual` initial
  

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec