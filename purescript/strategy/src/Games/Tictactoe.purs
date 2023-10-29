module EG.Games.Tictactoe where

import Prelude

import Data.Array (findMap, foldl, intercalate)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested (type (/\), (/\))
import Dodo (Doc)
import Dodo as DD
import EG.Data.Game (Game(..))
import EG.Data.Payoff (Payoff(..))
import EG.Data.Tuple as ETuple

---------- Types
data Piece = X | O

type Board = Array (Maybe Piece)
type Player = ETuple.TupleIndex

data TurnState = NextMove Player | WonBy Player | Draw

newtype State = State
  { board :: Board
  , turnState :: TurnState
  }

type GameGenerationState = { board :: Board, player :: Player }

type Choice = Int -- mod 9

type TictactoeGame = Game State Choice

---------- Helpers
initialState :: GameGenerationState
initialState =
  { board: Array.replicate 9 Nothing
  , player: ETuple.left
  }

pieceToPlayer :: Piece -> Player
pieceToPlayer X = ETuple.left
pieceToPlayer O = ETuple.right

playerToPiece :: Player -> Piece
playerToPiece player = ETuple.lookupPair player (X /\ O)

winner :: Board -> Maybe Player
winner = case _ of
  [ a11
  , a12
  , a13
  , a21
  , a22
  , a23
  , a31
  , a32
  , a33
  -- For 10 element version
  -- , a41
  ] ->
    patterns # findMap (allEqual >=> map pieceToPlayer)
    where
    patterns =
      [ [ a11, a12, a13 ]
      , [ a21, a22, a23 ]
      , [ a31, a32, a33 ]
      , [ a11, a21, a31 ]
      , [ a12, a22, a32 ]
      , [ a13, a23, a33 ]
      , [ a11, a22, a33 ]
      , [ a13, a22, a31 ]
      -- for weird version
      -- , [ a11, a23, a32 ]
      -- , [ a33, a12, a21 ]
      -- even more weird version
      -- , [ a13, a32, a21 ]
      -- , [ a31, a23, a12 ]

      -- for 10 element version
      -- , [ a21, a31, a41 ]
      -- , [ a23, a32, a41 ]

      -- for 12 element version
      -- , [ a41, a42, a43 ]
      -- , [ a21, a31, a41 ]
      -- , [ a22, a32, a42 ]
      -- , [ a23, a33, a43 ]
      -- , [ a21, a32, a43 ]
      -- , [ a23, a32, a41 ]
      ]
  _ -> Nothing
  where
  allEqual :: forall a. Eq a => Array a -> Maybe a
  allEqual = foldl go (Just Nothing) >>> join
    where
    go (Just Nothing) e = Just (Just e)
    go whole@(Just (Just p)) e
      | e == p = whole
    go _ _ = Nothing

---------- Game related functions
game :: TictactoeGame
game = go initialState
  where
  go { board, player }
    | Just player <- winner board = Game
        { state: State { board, turnState: WonBy player }
        , choices: []
        }
    | Array.all isJust board = Game
        { state: State { board, turnState: Draw }
        , choices: []
        }
    | otherwise = Game
        { state: State { board, turnState: NextMove player }
        , choices: do
            emptySpot <- board
              # Array.mapWithIndex (/\)
              # Array.mapMaybe \(index /\ piece) -> case piece of
                  Nothing -> Just index
                  Just _ -> Nothing

            let
              state =
                { player: ETuple.other player
                , board: board #
                    Array.modifyAtIndices
                      [ emptySpot ]
                      (const $ Just $ playerToPiece player)
                }

            pure (emptySpot /\ (go state))
        }

payoff :: Int -> State -> Maybe (Payoff /\ Payoff)
payoff depth (State { turnState }) = case turnState of
  WonBy player -> Just $ ETuple.lookupPair player ((pos /\ neg) /\ (neg /\ pos))
  Draw -> Just (mid /\ mid)
  _ -> Nothing
  where
  depth' = Int.toNumber depth
  pos = Payoff (1.0 / depth')
  mid = Payoff 0.0
  neg = Payoff (-1.0 / depth')

---------- Display stuff
displayChoice :: forall a. Choice -> Doc a
displayChoice num = Array.intercalate DD.space
  [ DD.text "Place a piece at"
  , DD.text "("
      <> DD.text (show (num `mod` 3))
      <> DD.text ","
      <> DD.space
      <> DD.text (show (num / 3))
      <> DD.text ")"
  ]

displayState :: forall a. State -> Doc a
displayState (State { board, turnState }) = DD.lines
  [ intercalate DD.space (map printPiece $ Array.slice 0 3 board)
  , intercalate DD.space (map printPiece $ Array.slice 3 6 board)
  , intercalate DD.space (map printPiece $ Array.slice 6 9 board)

  -- for 10 element version
  -- , intercalate DD.space (map printPiece $ Array.slice 9 10 board)
  , DD.text "State: " <> printTurnState turnState
  ]
  where
  printPiece (Just X) = DD.text "X"
  printPiece (Just O) = DD.text "O"
  printPiece Nothing = DD.text "-"

  printTurnState Draw = DD.text "Draw!"
  printTurnState (NextMove player) = Array.intercalate DD.space
    [ DD.text "player "
    , DD.text (ETuple.lookupPair player ("1" /\ "2"))
    , DD.text "should make a move"
    ]
  printTurnState (WonBy player) = Array.intercalate DD.space
    [ DD.text "won by - "
    , DD.text (ETuple.lookupPair player ("X player" /\ "O player"))
    ]

---------- Typeclass instances
derive instance Eq Piece

