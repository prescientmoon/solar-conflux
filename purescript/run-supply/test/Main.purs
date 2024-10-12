module Test.Main where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Run (Run, extract)
import Run.Supply (SUPPLY, generate, ignoreMaybes, localSupply, runSupply)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

add2 :: forall r. Run (SUPPLY Int r) Int
add2 = ado
  a <- generate
  b <- generate
  in a + b

add4 :: forall r. Run (SUPPLY Int r) Int
add4 = ado
  a <- add2
  b <- add2
  in a + b

spec :: Spec Unit
spec = describe "The supply effect" do
  describe "runSupply" do
    it "`generate + generate` should be the sum of the first 2 numbers" do
      let val = extract $ runSupply ((+) 3) 0 add2
      val `shouldEqual` 3 -- 0 + (0 + 3)
      let val' = extract $ runSupply ((+) 3) 4 add2
      val' `shouldEqual` 11 -- 4 + (4 + 3)
  describe "localSupply" do
    it "should allow mapping over the generated values" do
      let val = extract $ runSupply ((+) 1) 0 $ localSupply ((+) 4) add2
      val `shouldEqual` 9 -- (0 + 4) + ((0 + 1) + 4)
      let val' = extract $ runSupply ((+) 1) 4 $ localSupply ((*) (-1)) add2
      val' `shouldEqual` (-9) -- (-1 * 4) + (-1 * (4 + 1))
  describe "ignoreMaybes" do
    it "should work as usual if everything is a Just" do
      let val = extract $ runSupply (map ((+) 3)) (Just 0) $ ignoreMaybes add2
      val `shouldEqual` 3 -- 0 + (0 + 3)
    it "should skip over Nothings" do
      let runner = maybe (Just 2) (const Nothing)
      let val = extract $ runSupply runner Nothing $ ignoreMaybes add2
      val `shouldEqual` 4 -- 2 + 2
    it "should skip over mapped Nothings" do
      let withThrees m = extract $ runSupply ((<>) "3") "" $ localSupply fromString $ ignoreMaybes m
      (withThrees add2) `shouldEqual` 36 -- 3 + 33
      (withThrees add4) `shouldEqual` 3702 -- 3 + 33 + 333 + 333
  

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec