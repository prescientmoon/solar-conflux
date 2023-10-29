module Lunarline.String where

import Prelude

import Data.String (Pattern(..), joinWith, split)

-- | Indent a multi line string
indent :: Int -> String -> String
indent amount = split (Pattern "\n") >>> map (indentOne amount) >>> joinWith "\n"

-- | Indents a single line string
indentOne :: Int -> String -> String
indentOne n a
  | n <= 0 = a
  | otherwise = " " <> indentOne (n - 1) a

-- | Add parenthesis to a pretty printer when a condition holds
parensWhen :: forall a. (a -> Boolean) -> (a -> String) -> a -> String
parensWhen predicate print toPrint = if predicate toPrint then "(" <> print toPrint <> ")" else print toPrint