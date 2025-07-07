module Main where

import Nihil.Parser.Core (parseTest)
import Nihil.Parser.Type (pType)
import Relude

main ∷ IO ()
main = do
  let c = 20
  parseTest pType $ (fold $ replicate c "forall a,") <> "a"
  putStrLn "Hello, Haskell :3"
