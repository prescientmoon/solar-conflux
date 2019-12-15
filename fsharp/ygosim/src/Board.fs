module Board

open FSharpPlus.Lens

module Side =
    open Card.Card

    type Side<'s> =
        { field: CardInstance<'s> option
          monsters: CardInstance<'s> option list
          spells: CardInstance<'s> option list
          graveyard: CardInstance<'s> list
          deck: CardInstance<'s> list }

    module Side =
        let inline field f side = f side.field <&> fun v -> { side with field = v }
        let inline monsters f side = f side.monsters <&> fun v -> { side with monsters = v }
        let inline spells f side = f side.spells <&> fun v -> { side with spells = v }
        let inline graveyard f side = f side.graveyard <&> fun v -> { side with graveyard = v }
        let inline deck f side = f side.deck <&> fun v -> { side with deck = v }

    let emptyRow _ = List.init 5 <| fun _ -> None

    let emptySide _ =
        { field = None
          monsters = emptyRow()
          spells = emptyRow()
          graveyard = []
          deck = [] }


module Player =
    open Side
    open Card.Card

    type PlayerState =
        | InGame
        | Won of reason: string
        | Lost of reason: string

    type Player<'s> =
        { lifePoints: int
          side: Side<'s>
          hand: CardInstance<'s> list
          state: PlayerState
          id: int
          lastNormalSummon: int }

    module Player =
        let inline lifePoints f player = f player.lifePoints <&> fun v -> { player with lifePoints = v }
        let inline side f player = f player.side <&> fun v -> { player with side = v }
        let inline hand f player = f player.hand <&> fun v -> { player with hand = v }
        let inline state f player = f player.state <&> fun v -> { player with state = v }
        let inline _id f player = f player.id <&> fun v -> { player with id = v }
        let inline lastNormalSummon f player =
            f player.lastNormalSummon <&> fun v -> { player with lastNormalSummon = v }

        let inline deck f player = (side << Side.deck) f player
        let inline monsters f player = (side << Side.monsters) f player

    let initialPlayer lp id =
        { lifePoints = lp
          side = emptySide()
          hand = []
          state = InGame
          id = id
          lastNormalSummon = -1 }

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
        let inline currentPlayerState f board = (currentPlayer << Player.state) f board
        let inline currentPlayerDeck f board = (currentPlayer << Player.deck) f board
        let inline currentPlayerHand f board = (currentPlayer << Player.hand) f board
        let inline currentPlayerLastNormalSummon f board = (currentPlayer << Player.lastNormalSummon) f board
        let inline currentPlayerMonsters f board = (currentPlayer << Player.monsters) f board

        let inline firstPlayer f board = (players << _1) f board
        let inline secondPlayer f board = (players << _2) f board

    type Card = Card.Card<Board>

    type CardInstance = Card.CardInstance<Board>

    type Monster = Card.Monster<Board>

    type Effect = Effect.Effect<Board>

    type Condition = Effect.Condition<Board>

    type Action = Effect.Action<Board>

    let emptyBoard =
        { players = (initialPlayer 8000 0, initialPlayer 8000 1)
          moment = 0, Draw }

module Client =
    open Player
    open Turn

    type Log =
        | CardToHand of string
        | NewPhase of Phase
        | StateChanged of PlayerState * PlayerState
        | ChooseZone of int list

    type Client = Log -> int

    let rec chooseZone client free =
        let freeIndices = List.mapi (fun i _ -> i) free
        let command = ChooseZone freeIndices
        let result = client command

        if List.contains result freeIndices then free.[result]
        else chooseZone client free

module Zone =
    open Player
    open Side
    open Board

    let freeMonsterZones (player: Player) = List.filter Option.isNone player.side.monsters
    let freeMonsterZoneCount = freeMonsterZones >> List.length
    let hasFreeMonsterZones = (>=) << freeMonsterZoneCount
    let hasFreeMonsterZone player = hasFreeMonsterZones player 1


module Summon =
    open Card.Card
    open Card
    open Board
    open Zone
    open Client

    module Normal =
        let inline numberOfTributes (monster: Monster) =
            let level = monster ^. Card.level

            if level <= 4 then 0
            elif level <= 6 then 1
            else 2

        let isNormalSummonable board monster =
            let requiredTributes = numberOfTributes monster

            let possibleTributes =
                board ^. Board.currentPlayerMonsters
                |> List.filter Option.isSome
                |> List.length

            let freeZones = 5 - possibleTributes + requiredTributes

            requiredTributes <= possibleTributes && freeZones > 0


        let hasNormalSummonableMonster board =
            let hand = board ^. Board.currentPlayerHand
            let monsters = List.choose monster hand

            List.exists <| isNormalSummonable board <| monsters

        let canNormalSummon board =
            hasNormalSummonableMonster board && board ^. Board.currentPlayerLastNormalSummon < board ^. Board.turn

        let performNormalSummon client board =
            let free = freeMonsterZones <| board ^. Board.currentPlayer
            let zone = chooseZone client free

            let turn = board ^. Board.turn

            board |> Board.currentPlayerLastNormalSummon .-> turn

module Game =
    open Turn
    open Player
    open Board
    open Summon.Normal
    open Client

    let isCurrentPlayer (board: Board) (player: Player) = (board ^. Board.currentPlayerId) = player.id

    let canDrawCard (board: Board) (player: Player) =
        isCurrentPlayer board player && board ^. Board.phase = Draw && board ^. Board.turn <> 0

    let draw (board: Board) =
        match board ^. Board.currentPlayerDeck with
        | [] -> board |> Board.currentPlayerState .-> Lost "deckout"
        | card :: deck ->
            let hand = card :: (board ^. Board.currentPlayerHand)

            board
            |> Board.currentPlayerHand .-> hand
            |> Board.currentPlayerDeck .-> deck

    let toDeckBottom (card: CardInstance) (player: Player) = over Player.deck (fun d -> card :: d) player

    let handleMainPhase client board =
        if canNormalSummon board then performNormalSummon client board
        else board

    let processPhase client board =
        match board ^. Board.phase with
        | Draw ->
            if canDrawCard board <| board ^. Board.currentPlayer then draw board
            else board
        | Main1 -> handleMainPhase client board
        | Main2 -> handleMainPhase client board
        | _ -> board

    let switchPhases (client: Client) board =
        let newBoard = over Board.moment nextPhase board

        NewPhase <| newBoard ^. Board.phase
        |> client
        |> ignore

        newBoard


    let getPlayerStates board =
        (board ^. (Board.firstPlayer << Player.state), board ^. (Board.secondPlayer << Player.state))


    let resolvePlayerStates (p1, p2) =
        let s1, s2 = p1.state, p2.state

        match s1 with
        | Lost reason ->
            match s2 with
            | InGame -> p1, p2 |> Player.state .-> Won reason
            | _ -> p1, p2
        | Won reason ->
            match s2 with
            | InGame -> p1, p2 |> Player.state .-> Lost reason
            | _ -> p1, p2
        | InGame ->
            match s2 with
            | InGame -> p1, p2
            | Won reason -> p1 |> Player.state .-> Lost reason, p2
            | Lost reason -> p1 |> Player.state .-> Won reason, p2

    let resolveBoardState board = over Board.players resolvePlayerStates board

    let rec game board (client: Client) =
        let newBoard =
            (processPhase client)
            >> resolveBoardState
            <| board

        let currentState = newBoard ^. Board.currentPlayerState

        if currentState <> InGame then
            let newStates = getPlayerStates newBoard
            client <| StateChanged newStates |> ignore
        else
            game <| switchPhases client newBoard <| client
