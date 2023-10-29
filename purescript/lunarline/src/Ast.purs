module Lunarline.Ast where

import Prelude

import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Foldable (foldl)
import Data.Lens (_2, first, over)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Lunarline.String (indent, parensWhen)

---------- Types
data Case
  = Named String
  | Deconstruct String (Array Case)

data Expression
  = Var String
  | Lambda String Expression
  | Call Expression Expression
  | Match Expression (NonEmptyArray (Case /\ Expression))
  | Let String Expression Expression
  | Constructor String
  | IgnoreVar String Expression

---------- Helpers
-- | Syntax sugar for creating calls from purescript
callMany :: Expression -> Array Expression -> Expression
callMany f args = A.foldl Call f args

-- | Attempt organizing a constructor call 
-- | into the constructor name and the passed arguments
unrollConstructor :: Expression -> Maybe (String /\ Array Expression)
unrollConstructor (Call next argument)
  = unrollConstructor next
  <#> over _2 (flip Array.snoc argument)
unrollConstructor (Constructor name) = Just (name /\ [])
unrollConstructor _ = Nothing

-- | Checks if a pattern match cases shadows a given variable name
caseShadows :: String -> Case -> Boolean
caseShadows target (Named name) = target == name
caseShadows target (Deconstruct _ children) = A.any (caseShadows target) children

-- | Count the number of times a variable occurs inside an expression.
-- | Takes care of shadowing
occurences :: String -> Expression -> Int
occurences name = go 0
  where
  go acc (Var var) = if var == name then acc + 1 else acc
  go acc (IgnoreVar var expr) = if name == var then acc else go acc expr
  go acc (Lambda var expr) = if name == var then acc else go acc expr
  go acc (Call function argument) = go (go acc argument) function
  go acc (Constructor _) = acc
  go acc (Let var value body) = if var == name then inValue else go inValue body
    where
    inValue = go acc value
  go acc (Match expr cases) = foldl processCase (go acc expr) cases
    where
    processCase acc (thisCase /\ continuation) =
      if caseShadows name thisCase then
        acc else go acc continuation

-- | Ignore a var which references itself. Useful for avoiding infinite recursion while optimizing
avoidInfiniteRecursion :: String -> Expression -> Expression
avoidInfiniteRecursion var expr
  | occurences var expr == 0 = expr
  | otherwise = IgnoreVar var expr

---------- Pretty printing
instance Show Case where
  show (Named name) = name
  show (Deconstruct constructor nested) = constructor <> " "
    <> joinWith " " (parensWhen needsParens show <$> nested)
    where
    needsParens (Named _) = false
    needsParens (Deconstruct _ nested) = A.length nested /= 0

instance Show Expression where
  show (Var name) = name
  show (IgnoreVar var expr) =
    if occurences var expr /= 10000 then show expr
    else "shadow " <> show var <> ". " <> show expr
  show (Constructor name) = name
  show lam@(Lambda _ _) = "\\" <> joinWith " " args <> " -> " <> show body
    where
    args /\ body = collectLambdas lam
  show (Let name definition body) = "let " <> name <> " = " <> show definition <> "\nin " <> show body
  show (Match expr cases) = "case " <> parensWhen exprNeedsParens show expr <> " of" <> "\n" <> indent 2 prettyCases
    where
    exprNeedsParens = case _ of
      Match _ _ -> true
      IgnoreVar _ e -> exprNeedsParens e
      _ -> false
    prettyCases = joinWith "\n" $ NA.toArray $ printCase <$> cases
    printCase (thisCase /\ continuation) = show thisCase <> " ->" <>
      if onSeparateLine then "\n" <> indent 2 (show continuation)
      else " " <> show continuation
      where
      onSeparateLine = case continuation of
        Match _ _ -> true
        IgnoreVar _ _ -> true
        Lambda _ _ -> true
        Let _ _ _ -> true
        _ -> false
  show (Call function argument) =
    parensWhen leftNeedsParens show function
      <> " "
      <> parensWhen rightNeedsParens show argument
    where
    leftNeedsParens = case _ of
      Match _ _ -> true
      Lambda _ _ -> true
      IgnoreVar _ _ -> true
      Let _ _ _ -> true
      _ -> false

    rightNeedsParens = case _ of
      Call _ _ -> true
      IgnoreVar _ _ -> true
      _ -> false

-- | Gruop an expression into it's arguments and it's body.
-- | In case of non lambda expressions the argument array will be empty
collectLambdas :: Expression -> Array String /\ Expression
collectLambdas = go >>> first A.fromFoldable
  where
  go (Lambda name body) = first (List.Cons name) (go body)
  go other = List.Nil /\ other