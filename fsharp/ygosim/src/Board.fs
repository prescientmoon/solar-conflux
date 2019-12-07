module Board

module Side =
    open Card

    type Side =
        { field: CardInstance option
          monsters: CardInstance list
          spells: CardInstance list
          graveyard: CardInstance list
          deck: CardInstance list }

    let emptySide =
        { field = None
          monsters = []
          spells = []
          graveyard = []
          deck = [] }


module Player =
    open Side
    open Card

    type PlayerState =
        | InGame
        | Won
        | Lost of reason: string

    type Player =
        { lifePoints: int
          side: Side
          hand: CardInstance list
          state: PlayerState }

    let inflictDamage (player: Player) amount = { player with lifePoints = player.lifePoints - amount }

    let initialPlayer lp =
        { lifePoints = lp
          side = emptySide
          hand = []
          state = InGame }

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
    open Player
    open Turn

    type Board =
        { players: Player * Player
          turn: int
          phase: Phase }

    let emptyBoard =
        { players = (initialPlayer 8000, initialPlayer 8000)
          turn = 0
          phase = Turn.Draw }

module Game =
    open Board
    open Turn
    open Player
    open Card

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
