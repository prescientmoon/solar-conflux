module Moonlog.Substitution where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (fromMaybe)
import Moonlog.Expression (Ast(..), Expression)

type Substitution = HashMap String Expression

class Substituable a where
    substitute :: Substitution -> a -> a

instance substituableAst :: Substituable Expression where
    substitute subst v@(Var _ name) = fromMaybe v $ HashMap.lookup name subst 
    substitute subst (Constructor pattern) = Constructor pattern { arguments = map (substitute subst) pattern.arguments }

-- | More or less `append` for substitutions
mergeSubstitutions :: Substitution -> Substitution -> Substitution
mergeSubstitutions s1 s2 = foldlWithIndex (\name subst term -> insert name term subst) s1 s2

-- | Insert a new var-term pair into a substitution
insert :: String -> Expression -> Substitution -> Substitution
insert name term s = HashMap.insert name term $ map updateMap s
    where
    updateMap = substitute $ HashMap.singleton name term