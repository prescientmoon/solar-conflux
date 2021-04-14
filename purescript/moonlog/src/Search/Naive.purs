module Moonlog.Search.Naive where

import Prelude

import Data.Array (concat, filter, mapMaybe, uncons)
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Moonlog.Expression (Constructor, Expression, Generalized, Rule, instantiateRule, mapPattern, runInstantiation)
import Moonlog.Substitution (Substitution, mergeSubstitutions, substitute)
import Moonlog.Unify (unifyMany)
import Run (Run, extract)
import Run.Except (runExcept)
import Run.Reader (READER, ask)
import Run.Supply (SUPPLY)
import Type.Row (type (+))

type Rules = HashMap String (Array (Rule Generalized))
type SOLVE r = READER Rules + SUPPLY Int r

getMatchingRules :: forall r. Constructor Expression -> Run (SOLVE r) (Array (Rule Expression /\ Substitution))
getMatchingRules constructor = do
    rules <- ask <#> HashMap.lookup constructor.name <#> fromMaybe []
    instantiated <- for rules (instantiateRule >>> runInstantiation)
    -- traceM $ "Matchin rules: " <> show instantiated
    pure $ flip mapMaybe instantiated \rule -> case extract $ runExcept $ unifyMany constructor.arguments rule.head.arguments of
        Left v -> do
            -- traceM $ "Failed to unify\n    " <> show constructor <> "\nwith\n    " <> show rule.head <> "\nwith error: " <> show ((fst v) :: Variant (UnificationErrors ()))
            Nothing
        Right subst -> do
            -- traceM $ "Succesfully unified " <> show rule <> " with subst " <> show subst
            Just $ rule /\ subst 

-- TODO: unconsing on lists like this is kinda bad
branch :: forall r. Array (Constructor Expression) -> Run (SOLVE r) (Array (Substitution /\ Array (Constructor Expression))) 
branch = uncons >>> case _ of
    Nothing -> pure []
    Just { head, tail } -> do
        rules <- getMatchingRules head
        pure $ rules <#> \(rule /\ subst) -> subst /\ map (mapPattern $ substitute subst) (rule.body <> tail)

solve :: forall r. Array (Constructor Expression) -> Run (SOLVE r) (Array Substitution)
solve [] = pure [HashMap.empty]
solve goals = do
    -- traceM $ "Solving " <> show goals
    branches <- branch goals
    -- traceM $ "Finished solving " <> show goals
    concat <$> for branches \(subst /\ goals') -> do
        solutions <- solve goals'
        -- traceM { goals, branches, goals', subst, solutions }
        -- TODO: safe substitution merging
        pure $ mergeSubstitutions subst <$> solutions

printResults :: Array Substitution -> Array String -> String
printResults [] _ = "false"
printResults solutions freeVars = joinWith "\n" $ 
    solutions # filter (HashMap.isEmpty >>> not) <#> 
        \solution -> joinWith "\n" $ freeVars <#> 
            \var -> "    " <> var <> " = " <> maybe "???" show (HashMap.lookup var solution)
