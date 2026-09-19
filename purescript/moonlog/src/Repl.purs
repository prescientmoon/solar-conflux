module Moonlog.Repl where

import Prelude

import Data.Array (foldl, uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String.Utils (words)
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Moonlog.Expression (Ast(..), Constructor, Expression, Generalized, Rule, constructorFromCst, freeVars, generalizeRule, ruleFromCst, runGeneralization)
import Moonlog.Parser.Cst (parse, parseConstructor)
import Moonlog.Search.Naive (printResults, solve)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.ReadLine (Interface)
import Node.ReadLine.Aff (prompt)
import Run (extract)
import Run.Reader (runReader)
import Run.Supply (runSupply)
import Type.Proxy (Proxy(..))


data Command
    = Load String
    | AddRule (Rule Expression)
    | Query (Constructor Expression)
    | Quit

type ReplState =
    { rules :: HashMap String (Array (Rule Generalized))
    , interface :: Interface }

addRule :: Rule Expression -> ReplState -> ReplState
addRule rule = over _rules $ HashMap.insertWith (<>) (rule.head.name) [extract $ runGeneralization $ generalizeRule rule]

loop :: ReplState -> Aff Unit
loop state = do
    input <- prompt state.interface
    case uncons $ words input of
        Just { head: ":quit" } -> pure unit
        Just { head: ":load", tail: paths } -> do
            rules <- join <$> for paths \path -> do
                text <- readTextFile UTF8 path
                -- traceM $ parse text
                case parse text of
                    Left err -> log ("ParsingError: " <> err.message) $> []
                    Right rules -> pure rules
            loop $ foldl (flip addRule) state $ ruleFromCst <$> rules
        _ | trim input == "" -> loop state
          | otherwise -> do
            case parseConstructor input of
                Left err -> log err.message *> loop state
                Right rawQuery -> do
                    let query = constructorFromCst rawQuery
                    let solutions = extract $ runSupply ((+) 1) 0 $ runReader state.rules $ solve [query]
                    log $ printResults solutions $ Array.fromFoldable $ freeVars $ Constructor query
                    loop state

emptyState :: Interface -> ReplState
emptyState = { rules: HashMap.empty, interface: _ }

---------- Lenses
_rules :: Lens' ReplState (HashMap String (Array (Rule Generalized)))
_rules = prop (Proxy :: _ "rules")