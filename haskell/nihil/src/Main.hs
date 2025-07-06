module Main where

import Nihil.Parser.Core (parseTest)
import Nihil.Parser.Type (type')
import Relude

main ∷ IO ()
main = do
  let c = 20
  parseTest type' $ (fold $ replicate c "forall a,") <> "a"
  putStrLn "Hello, Haskell :3"
