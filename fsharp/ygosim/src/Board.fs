module Board

module Side =
    open Card.Card

    type Side<'s> =
        { field: CardInstance<'s> option
          monsters: CardInstance<'s> list
          spells: CardInstance<'s> list
          graveyard: CardInstance<'s> list
          deck: CardInstance<'s> list }

    let emptySide =
        { field = None
          monsters = []
          spells = []
          graveyard = []
          deck = [] }


module Player =
    open Side
    open Card.Card

    type PlayerState =
        | InGame
        | Won
        | Lost of reason: string

    type Player<'s> =
        { lifePoints: int
          side: Side<'s>
          hand: CardInstance<'s> list
          state: PlayerState }

    let initialPlayer lp =
        { lifePoints = lp
          side = emptySide
          hand = []
          state = InGame }

// module Side =

module Turn =
    type Phase =
        | Draw
        | Standby
        | Main1
        | Battle
        | Main2
        | End

    let nextPhase (previous: Phase) (turn: int) =
        match previous with
        | Draw -> (Standby, turn)
        | Standby -> (Main1, turn)
        | Main1 -> (Battle, turn)
        | Battle -> (Main2, turn)
        | Main2 -> (End, turn)
        | End -> (Draw, turn + 1)

module Board =
    open Turn
    open Card

    type Player = Player.Player<Board>

    and Board =
        { players: Player * Player
          turn: int
          phase: Phase }

    type Card = Card.Card<Board>

    type CardInstance = Card.CardInstance<Board>

    type Effect = Effect.Effect<Board>

    type Condition = Effect.Condition<Board>

    type Action = Effect.Action<Board>

    let emptyBoard =
        { players = (Player.initialPlayer 8000, Player.initialPlayer 8000)
          turn = 0
          phase = Draw }

module Game =
    open Board
    open Turn
    open Player

    type PlayerAction =
        | Pass
        | NormalSummon
        | InitialDraw
        | Activate
        | Set

    // let canDoInitialDraw (board: Board) =


    let draw (player: Player) =
        match player.side.deck with
        | [] -> { player with state = Lost "deckout" }
        | card :: deck ->
            { player with
                  hand = card :: player.hand
                  side = { player.side with deck = deck } }

    // Player is the last arg to be able to use this with the withCurrentPlayer function
    let toDeckBottom (card: CardInstance) (player: Player) =
        { player with side = { player.side with deck = card :: player.side.deck } }

    let currentPlayer (board: Board) =
        let (first, second) = board.players

        if board.turn % 2 = 0 then first
        else second

    let withCurrentPlayer callback board =
        let (first, second) = board.players

        let players =
            if board.turn % 2 = 0 then (callback first, second)
            else (first, callback second)

        { board with players = players }


    let processTurn (board: Board) =
        match board.phase with
        | Draw -> withCurrentPlayer draw board
        | _ -> board

    let doTurn (board: Board) =
        let newBoard = processTurn board
        let (phase, turn) = nextPhase newBoard.phase newBoard.turn

        { newBoard with
              turn = turn
              phase = phase }
