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
          state: PlayerState
          id: int
          lastInitialDraw: int }

    module Player =
        let inline lifePoints f player = f player.lifePoints <&> fun v -> { player with lifePoints = v }
        let inline side f player = f player.side <&> fun v -> { player with side = v }
        let inline hand f player = f player.hand <&> fun v -> { player with hand = v }
        let inline state f player = f player.state <&> fun v -> { player with state = v }
        let inline _id f player = f player.id <&> fun v -> { player with id = v }
        let inline lastInitialDraw f player = f player.lastInitialDraw <&> fun v -> { player with lastInitialDraw = v }

        let inline deck f player = (side << Side.deck) f player

    let initialPlayer lp id =
        { lifePoints = lp
          side = emptySide
          hand = []
          state = InGame
          id = id
          lastInitialDraw = -1 }

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
    open Player

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

        let inline currentPlayerId f board = (currentPlayer << Player._id) f board
        let inline currentPlayerLastDraw f board = (currentPlayer << Player.lastInitialDraw) f board
        let inline currentPlayerState f board = (currentPlayer << Player.state) f board
        let inline currentPlayerDeck f board = (currentPlayer << Player.deck) f board
        let inline currentPlayerHand f board = (currentPlayer << Player.hand) f board

    type Card = Card.Card<Board>

    type CardInstance = Card.CardInstance<Board>

    type Effect = Effect.Effect<Board>

    type Condition = Effect.Condition<Board>

    type Action = Effect.Action<Board>

    let emptyBoard =
        { players = (Player.initialPlayer 8000 0, Player.initialPlayer 8000 1)
          moment = 0, Draw }

module Game =
    open Turn
    open Player
    open Side
    open Board

    type PlayerAction =
        | Pass
        | NormalSummon
        | InitialDraw
        | Activate
        | Set

    type ClientCommand = Log of string

    type ClientResult =
        | Zone of int
        | Bool of int
        | NoResult

    type Client = ClientCommand -> ClientResult

    let isCurrentPlayer (board: Board) (player: Player) = (board ^. Board.currentPlayerId) = player.id

    let canDrawCard (board: Board) (player: Player) =
        isCurrentPlayer board player && board ^. Board.currentPlayerLastDraw <> board ^. Board.turn
        && board ^. Board.phase = Draw && board ^. Board.turn <> 0


    let draw (board: Board) =
        match board ^. Board.currentPlayerDeck with
        | [] -> board |> Board.currentPlayerState .-> Lost "deckout"
        | card :: deck ->
            let hand = card :: (board ^. Board.currentPlayerHand)
            let turn = board ^. Board.turn

            board
            |> Board.currentPlayerHand .-> hand
            |> Board.currentPlayerDeck .-> deck
            |> Board.currentPlayerLastDraw .-> turn

    let toDeckBottom (card: CardInstance) (player: Player) = over Player.deck (fun d -> card :: d) player

    let processAction (client: Client) (board: Board) (player: Player) (action: PlayerAction) =
        match action with
        | InitialDraw ->
            if canDrawCard board player then
                (draw board, true)
            else
                client <| Log "cannot draw card" |> ignore
                (board, false)
        | Pass ->
            if isCurrentPlayer board player then (over Board.moment nextPhase board, true)
            else (board, false)
        | _ -> (board, true)
