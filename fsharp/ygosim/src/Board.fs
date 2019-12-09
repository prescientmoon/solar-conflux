module Board

open FSharpPlus.Lens

module Side =
    open Card.Card

    type Side<'s> =
        { field: CardInstance<'s> option
          monsters: CardInstance<'s> list
          spells: CardInstance<'s> list
          graveyard: CardInstance<'s> list
          deck: CardInstance<'s> list }

    module Side =
        let inline field f side = f side.field <&> fun v -> { side with field = v }
        let inline monsters f side = f side.monsters <&> fun v -> { side with monsters = v }
        let inline spells f side = f side.spells <&> fun v -> { side with spells = v }
        let inline graveyard f side = f side.graveyard <&> fun v -> { side with graveyard = v }
        let inline deck f side = f side.deck <&> fun v -> { side with deck = v }

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

    module Player =
        let inline lifePoints f player = f player.lifePoints <&> fun v -> { player with lifePoints = v }
        let inline side f player = f player.side <&> fun v -> { player with side = v }
        let inline hand f player = f player.hand <&> fun v -> { player with hand = v }
        let inline state f player = f player.state <&> fun v -> { player with state = v }
        let inline deck f player = (side << Side.deck) f player

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

    let nextPhase (turn, phase) =
        match phase with
        | Draw -> (turn, Standby)
        | Standby -> (turn, Main1)
        | Main1 -> (turn, Battle)
        | Battle -> (turn, Main2)
        | Main2 -> (turn, End)
        | End -> (turn + 1, Draw)

module Board =
    open Turn
    open Card

    type Player = Player.Player<Board>

    and Board =
        { players: Player * Player
          moment: int * Phase }

    module Board =
        let inline players f board = f board.players <&> fun v -> { board with players = v }
        let inline moment f board = f board.moment <&> fun v -> { board with moment = v }

        let inline turn f board = (moment << _1) f board
        let inline phase f board = (moment << _2) f board

        let inline currentPlayer f board =
            if (view turn board) % 2 = 0 then (players << _2) f board
            else (players << _1) f board

    type Card = Card.Card<Board>

    type CardInstance = Card.CardInstance<Board>

    type Effect = Effect.Effect<Board>

    type Condition = Effect.Condition<Board>

    type Action = Effect.Action<Board>

    let emptyBoard =
        { players = (Player.initialPlayer 8000, Player.initialPlayer 8000)
          moment = 0, Draw }

module Game =
    open Board
    open Turn
    open Player
    open Side

    type PlayerAction =
        | Pass
        | NormalSummon
        | InitialDraw
        | Activate
        | Set


    let draw (player: Player) =
        match player.side.deck with
        | [] -> player |> Player.state .-> Lost "deckout"
        | card :: deck ->
            let hand = card :: player.hand

            player
            |> Player.hand .-> hand
            |> Player.deck .-> deck

    // Player is the last arg to be able to use this with the withCurrentPlayer function
    let toDeckBottom (card: CardInstance) (player: Player) = over Player.deck (fun d -> card :: d) player

    let processTurn (board: Board) =
        match board ^. Board.phase with
        | Draw -> over Board.currentPlayer draw board
        | _ -> board

    let doTurn (board: Board) = over Board.moment nextPhase <| processTurn board
