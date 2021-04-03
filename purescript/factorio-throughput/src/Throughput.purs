module RealFunction where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldMap, for_, minimum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashMap as Map
import Data.HashSet as HashSet
import Data.Int (toNumber)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Functorio.Lens (modifyAt)
import Math (sin)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Run (Run, extract)
import Run.Except (EXCEPT, fail, runExcept)
import Run.Fail.Extra (traverseFail)
import Run.Reader (READER, ask, runReader)
import Run.Reader.Extra (fromState')
import Run.State (STATE, runState)
import Run.Supply (SUPPLY, generate, runSupply)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Visited (VISITED, once, runVisited)

type RealFunction = Number -> Number
type BeltConfig = 
    { speed :: Number
    , delay :: Number }

type ChestConfig =
    { maxContent :: Number
    , delay :: Number }

type PortId = Int
type MachineId = Int

data PortSide = Input | Output

data Machine
    = Belt { input :: PortId, output :: PortId, config :: BeltConfig }
    | Chest { inputs :: Array PortId, outputs :: Array PortId, config :: ChestConfig }
    | Provider (Array PortId) RealFunction
    | Consumer PortId

type Factory = HashMap MachineId Machine

---------- Some configs
yellowBelt :: BeltConfig
yellowBelt = { speed: 15.0, delay: 4.0/3.0 }

redBelt :: BeltConfig
redBelt = { speed: 30.0, delay: 4.0/6.0 }

blueBelt :: BeltConfig
blueBelt = { speed: 45.0, delay: 4.0/8.0 }

-- | Example factory
myFactory :: Factory
myFactory = Map.fromArray machines
    where
    machines = mapWithIndex Tuple 
        [ Provider [0, 1] $ startsAtZero $ \t -> 40.0 + 10.0 * sin t
        , Belt { input: 0, output: 3, config: yellowBelt }
        , Belt { input: 1, output: 4, config: redBelt }
        -- , Belt { input: 2, output: 5, config: blueBelt }
        , Consumer 3
        , Consumer 4
        ]

---------- Helpers for real functions
type Endomorphism a = a -> a

startsAtZero :: Endomorphism RealFunction
startsAtZero f x | x >= 0.0 = f x
                 | otherwise = 0.0

---------- Monad for factory solving
type PortData =
    { id :: PortId
    , maxInput :: Number -> Number
    , maxOutput :: Number -> Number }

data ConstraintExpression
    = PortDependent (Array PortId) (Array PortData -> RealFunction)
    | Function RealFunction
    | Literal Number

type BiRelationship =         
    { p1top2 :: RealFunction
    , p2top1 :: RealFunction
    , p1 :: PortId /\ PortSide
    , p2 :: PortId /\ PortSide }

type BiRelationshipId = Int

data ThroughputConstraint
    = Limit ConstraintExpression PortSide PortId
    | BiRelationship BiRelationshipId BiRelationship

type Constraints = Array ThroughputConstraint

type SolveState = 
    { constraints :: Constraints }

type SolveM = Run 
    ( EXCEPT String 
    + STATE SolveState 
    + READER Factory
    + SUPPLY Int
    + () )

runSolveM :: forall a. Factory -> SolveM a -> Either String (Tuple SolveState a)
runSolveM factory = runReader factory >>> runState mempty >>> runExcept >>> runSupply ((+) 1) 0 >>> extract

focusBiRelationship :: PortId /\ PortSide -> BiRelationship -> Maybe BiRelationship
focusBiRelationship place relationship | relationship.p1 == place = Just relationship
                                       | relationship.p2 == place = Just $ flipBiRelationship relationship
                                       | otherwise = Nothing 

focusBiRelationshipWithoutSide :: PortId -> BiRelationship -> Maybe BiRelationship
focusBiRelationshipWithoutSide id relationship | fst relationship.p1 == id = Just relationship
                                               | fst relationship.p2 == id = Just $ flipBiRelationship relationship
                                               | otherwise = Nothing

flipBiRelationship :: BiRelationship -> BiRelationship 
flipBiRelationship { p1, p2, p1top2, p2top1 } = { p1: p2, p2: p1, p1top2: p2top1, p2top1: p1top2 }

factoryPorts :: Factory -> HashSet.HashSet PortId
factoryPorts = foldMap case _ of
    Belt { input, output } -> HashSet.fromArray [input, output]
    Provider outputs _ -> HashSet.fromArray outputs
    Chest { inputs, outputs } -> HashSet.fromArray (inputs <> outputs)
    Consumer input -> HashSet.singleton input

---------- System solving algorithm
constrain :: ThroughputConstraint -> SolveM Unit
constrain constraint = modifyAt _constraints $ push constraint
    where
    push = flip Array.snoc

collectConstraints :: SolveM Unit
collectConstraints = do
    factory <- ask
    for_ (HashMap.toArrayBy (/\) $ factory) $ uncurry collectConstraintsImpl

getPortData :: forall r. PortId -> Run (READER Constraints r) PortData
getPortData id = ado 
    maxInput <- tryFindBound $ id /\ Input
    maxOutput <- tryFindBound $ id /\ Output
    in { id, maxInput, maxOutput }

evalExpr :: forall r. ConstraintExpression -> Run (READER Constraints r) RealFunction
evalExpr = case _ of
    Literal a -> pure (const a)
    Function f -> pure f
    PortDependent portIds f -> for portIds getPortData <#> f

tryFindBound :: forall r. PortId /\ PortSide -> Run (READER Constraints r) RealFunction
tryFindBound at = tryFindBoundImpl at <#> \f time -> extract $ runVisited $ f time 

tryFindBoundSolveM :: PortId /\ PortSide -> SolveM RealFunction
tryFindBoundSolveM at = fromState' _constraints $ tryFindBound at

tryFindBoundPure :: PortId /\ PortSide -> Constraints -> RealFunction
tryFindBoundPure at constraints = extract $ runReader constraints $ tryFindBound at

tryFindBoundImpl :: forall r k. 
    PortId /\ PortSide -> 
    Run (READER Constraints r) (Number -> Run (VISITED BiRelationshipId k) Number) 
tryFindBoundImpl (targetId /\ targetSide) = do
    constraints <- ask
    pure \time -> constraints
        # traverseFail case _ of
            Limit expr side id | side == targetSide && id == targetId -> 
                evalExpr expr <*> pure time 
            BiRelationship id raw 
                | Just relationship <- focusBiRelationship (targetId /\ targetSide) raw -> do
                    f <- once id fail $ tryFindBoundImpl relationship.p2 
                    f (relationship.p1top2 time)
            _ -> fail
        # runReader constraints 
        <#> minimum'
    where
    minimum' = minimum >>> fromMaybe 0.0

tryFindValue :: forall r. PortId -> Run (READER Constraints r) RealFunction
tryFindValue at = tryFindValueImpl at <#> \f time -> extract $ runVisited $ f time 

tryFindValueImpl :: forall r k. PortId -> Run (READER Constraints r) (Number -> Run (VISITED BiRelationshipId k) Number)
tryFindValueImpl targetId = do
    constraints <- ask
    pure \time -> constraints
        # traverseFail case _ of
            Limit expr _ id | id == targetId -> evalExpr expr <*> pure time 
            BiRelationship id raw 
                | Just relationship <- focusBiRelationshipWithoutSide targetId raw -> do
                    f <- once id fail $ tryFindValueImpl $ fst relationship.p2 
                    f (relationship.p1top2 time)
            _ -> fail
        # runReader constraints 
        <#> minimum'
    where
    minimum' = minimum >>> fromMaybe 0.0

tryFindValuePure :: PortId -> Constraints -> RealFunction
tryFindValuePure at constraints = extract $ runReader constraints $ tryFindValue at

collectConstraintsImpl :: MachineId -> Machine -> SolveM Unit
collectConstraintsImpl at = case _ of
    Provider for amount -> do
        forWithIndex_ for \index id -> do
            let limit ports time 
                  = outputs ports time
                  # Array.findMap (\(id' /\ f) -> if id == id' then Just (f time) else Nothing)
                  # unsafePartial fromJust -- TODO: error handling
            constrain $ Limit (PortDependent for limit) Input id
        where
        outputs :: Array PortData -> Number -> Array (PortId /\ RealFunction)
        outputs ports time 
            = outputsImpl (length ports) (List.fromFoldable sorted) amount 
            # Array.fromFoldable 
            # Array.zipWith (_.id >>> Tuple) sorted
            where
            sorted :: Array PortData
            sorted = Array.sortWith (_.maxOutput >>> (#) time) ports

        outputsImpl :: Int -> List PortData -> RealFunction -> List RealFunction 
        outputsImpl 1 (head:Nil) remaining = pure \time -> min (head.maxOutput time) (remaining time)
        outputsImpl n (head:tail) remaining = current:(outputsImpl (n - 1) tail $ remaining - current)
            where
            current time 
                | head.maxOutput time >= (remaining time) / (toNumber n) = (remaining time) / (toNumber n)
                | otherwise = head.maxOutput time
        outputsImpl _ _ _ = Nil

    Consumer for -> do
        constrain $ Limit (Literal infinity) Output for
    Belt { input, output, config } -> do
        biId <- generate

        constrain $ BiRelationship biId 
            { p1: input /\ Output
            , p2: output /\ Input
            , p1top2: (+) config.delay
            , p2top1: (+) (-config.delay) }

        constrain $ Limit (Literal config.speed) Output input
        constrain $ Limit (Literal config.speed) Input output

    _ -> unsafeCrashWith "unimplemented"

---------- Lenses
_constraints :: Lens' SolveState (Array ThroughputConstraint) 
_constraints = prop (Proxy :: _ "constraints")

---------- Typeclass instances
derive instance genericMachine :: Generic Machine _
derive instance genericPortSide :: Generic PortSide _
derive instance eqPortSide :: Eq PortSide

instance showMachine :: Show Machine where
    show = case _ of
        Provider for _ -> "Provider<" <> show for <> ">"
        Consumer for -> "Consumer<" <> show for <> ">"
        Belt { config, input, output } -> "Belt<" <> show input <> " -> " <> show output <> ", " <> show config <> ">"
        Chest { inputs, outputs, config } -> "Chest<" <> show inputs <> " -> " <> show outputs <> ", " <> show config <> ">"

instance showConstraint :: Show ThroughputConstraint where
    show = case _ of
        Limit f side id -> show f <> " !> " <> showPort (id /\ side)
        BiRelationship _ { p1, p2 } -> showPort p1 <> " <-> " <> showPort p2
        where
        showPort (p /\ side) = "?" <> show p <> case side of
            Input -> "<-"
            Output -> "<-"

instance showConstraintExpression :: Show ConstraintExpression where
    show (Literal i) = show i
    show (Function f) = "<Function>"
    show (PortDependent ids f) = "(" <> show ids <> " -> <Function>)"  

instance showPortSide :: Show PortSide where
    show = genericShow