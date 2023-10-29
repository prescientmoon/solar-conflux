module EG.Data.Game where

import Prelude

import Data.Number as Number
import Data.Array as Array
import Data.Foldable (maximum, maximumBy, sum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (infinity)
import Data.Profunctor.Strong (second, (&&&))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Dodo (Doc)
import Dodo as DD
import EG.Data.Payoff (Payoff(..))
import EG.Data.Tuple (TupleIndex)
import EG.Data.Tuple as ETuple
import Effect (Effect)
import Effect.Random (randomInt)
import Safe.Coerce (coerce)

type Player = TupleIndex

newtype Game state choice = Game
  { state :: state
  , choices :: Array (choice /\ (Game state choice))
  }

newtype GameReplay state choice = GameReplay
  { state :: state
  , next :: Maybe (choice /\ GameReplay state choice)
  }

newtype SolvedState state choice = SolvedState
  { state :: state
  , choice :: Maybe (choice /\ Game (SolvedState state choice) choice)
  , payoffs :: Maybe (Payoff /\ Payoff)
  }

type PayoffFunction s = Int -> s -> Maybe (Payoff /\ Payoff)

---------- Helpers
solvedStatePayoffs :: forall s c. SolvedState s c -> Maybe (Payoff /\ Payoff)
solvedStatePayoffs (SolvedState { payoffs }) = payoffs

gameNodeCount :: forall s c. Game s c -> Int
gameNodeCount (Game { choices }) = 1 + sum (map (gameNodeCount <<< snd) choices)

gameLeafCount :: forall s c. Game s c -> Int
gameLeafCount (Game { choices: [] }) = 1
gameLeafCount (Game { choices }) = sum (map (gameLeafCount <<< snd) choices)

gameDepth :: forall s c. Game s c -> Int
gameDepth (Game { choices }) = maybe 0 ((+) 1)
  $ maximum
  $ map (snd >>> gameDepth) choices

randomGame :: forall s c. Game s c -> Effect (GameReplay s c)
randomGame (Game { choices, state }) = do
  index <- randomInt 0 (Array.length choices - 1)
  next <- case Array.index choices index of
    Nothing -> pure Nothing
    Just (choice /\ next) -> do
      next <- randomGame next
      pure (Just (choice /\ next))
  pure $ GameReplay { state, next }

displayReplay :: forall s c a. (s -> Doc a) -> (c -> Doc a) -> GameReplay s c -> Doc a
displayReplay printState printChoice (GameReplay { state, next }) =
  DD.lines
    [ DD.indent (printState state)
    , case next of
        Nothing -> mempty
        Just (choice /\ next) ->
          DD.lines
            [ Array.intercalate DD.space
                [ DD.text "----"
                , printChoice choice
                , DD.text "---->"
                ]
            , displayReplay printState printChoice next
            ]

    ]

gameState :: forall s c. Game s c -> s
gameState (Game { state }) = state

computePayoffs
  :: forall s c
   . Player
  -> PayoffFunction s
  -> Game s c
  -> Game (SolvedState s c) c
computePayoffs player calculatePayoffs game = go player 0 game
  where
  go player depth (Game { state, choices }) = case choices of
    [] -> Game
      { choices: []
      , state: SolvedState
          { state
          , payoffs: calculatePayoffs depth state
          , choice: Nothing
          }
      }
    choices -> Game
      { choices: nested
      , state: SolvedState
          { state
          , payoffs: map _.payoff optimalChoice
          , choice: map (_.choice &&& _.game) optimalChoice
          }
      }
      where
      nested = choices
        # map (second (go (ETuple.other player) (depth + 1)))

      optimalChoice = nested
        # map
            ( \(choice /\ game) ->
                { choice
                , game
                , payoff: game
                    # gameState
                    # solvedStatePayoffs
                    # fromMaybe (hell /\ hell)
                }
            )
        # maximumBy (on compare (_.payoff >>> ETuple.lookupPair player))

      hell = Payoff (-infinity)

-- minmax :: forall s c. PayoffFunction s -> Player -> Game s c -> Maybe (c /\ Game s c)
-- minmax calculatePayoffs player (Game { choices, state }) =
--   choices
--     # Array.mapMaybe
--         ( \(choice /\ game) -> case calculatePayoffs state of
--             Just payoffs -> Just { choice, game, payoffs }
--             Nothing -> Nothing
--         )
--     # maximumBy (on compare (_.payoffs >>> ETuple.lookup player))
--     # map \{ choice, game } -> choice /\ game

---------- Display stuff
displaySolvedState :: forall a s c. (s -> Doc a) -> (c -> Doc a) -> SolvedState s c -> Doc a
displaySolvedState displayState displayChoice (SolvedState { payoffs, choice, state }) = DD.lines
  [ DD.indent (displayState state)
  , Array.intercalate DD.space
      [ DD.text "Payoffs:"
      , displayUnknown displayPayoffs payoffs
      ]
  , Array.intercalate DD.space
      [ DD.text "Optimal choice:"
      , displayUnknown displayChoice (map fst choice)
      ]
  ]

  where
  displayUnknown :: forall e. (e -> Doc a) -> Maybe e -> Doc a
  displayUnknown f (Just a) = f a
  displayUnknown f Nothing = DD.text "???"

  displayPayoffs (l /\ r) = Array.fold
    [ DD.text "("
    , displayNum (coerce l)
    , DD.text ","
    , DD.space
    , displayNum (coerce r)
    , DD.text ")"
    ]
    where
    displayNum num = DD.text (show $ Number.floor (num * precision) / precision)
    precision = 1000.0

-- newtype Payoff = Payoff Int
--
-- newtype PayoffTreeNode label state choice = PayoffTreeNode
--   { expectedPayoff :: Payoff /\ Payoff
--   , children :: PayoffTree label state choicehoi-- 
--
-- newtype PayoffTree label state choice = PayoffTree
--   { label :: label
--   , state :: state
--   , choices :: Array (PayoffTreeNode label state choice)
--   }

-- minmax
--   :: forall label choice
--    . Foldable choice
--   => Player
--   -> PayoffTree label choice
--   -> PayoffTreeNode label choice
-- minmax player (PayoffTree { label, choices }) = choice
--   where
--   choice = choices # maximumBy
--     ( on compare \(PayoffTreeNode { expectedPayoff, children }) ->
--         ETuple.lookup player expectedPayoff
--     )

---------- Typeclass instances
-- derive instance Eq Payoff
-- derive instance Ord Payoff
