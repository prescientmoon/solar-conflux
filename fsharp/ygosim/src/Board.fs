module Board

open FSharpPlus.Lens
open FSharpPlus.Operators

module Side =
    open Card.CardInstance

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
    open Card.CardInstance

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
        let inline graveyard f player = (side << Side.graveyard) f player

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
          moment: int * Phase
          lastInstanceId: int }

    module Board =
        let inline players f board = f board.players <&> fun v -> { board with players = v }
        let inline moment f board = f board.moment <&> fun v -> { board with moment = v }
        let inline lastInstanceId f board = f board.lastInstanceId <&> fun v -> { board with lastInstanceId = v }

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
        let inline currentPlayerSide f board = (currentPlayer << Player.side) f board

        let inline firstPlayer f board = (players << _1) f board
        let inline secondPlayer f board = (players << _2) f board

    type Card = Card.Card<Board>

    type CardInstance = CardInstance.CardInstance<Board>

    type Monster = MonsterTypes.Monster<Board>

    type MonsterInstance = MonsterInstance.MonsterInstance<Board>

    type Effect = Effect.Effect<Board>

    type Condition = Effect.Condition<Board>

    type Action = Effect.Action<Board>

    let emptyBoard =
        { players = (initialPlayer 8000 0, initialPlayer 8000 1)
          moment = 0, Draw
          lastInstanceId = -1 }

    let instantiate board card =
        let instance =
            { CardInstance.template = card
              CardInstance.id = board.lastInstanceId }

        (instance, over Board.lastInstanceId <| (+) 1 <| board)

module Client =
    open Player
    open Turn
    open Card
    open Board
    open Utils


    type Log =
        | CardToHand of string
        | MonsterSummoned of Monster * int
        | NewPhase of Phase
        | StateChanged of PlayerState * PlayerState
        | ChooseZone of int list
        | ChooseMonster of Monster list
        | ChooseTributes of Monster list

    type Client = Log -> int

    let rec chooseZone client free =
        let result =
            free |>> view _1
            |> ChooseZone
            |> client

        if List.containsIndex result free then free.[result] ^. _1
        else chooseZone client free

    let rec chooseMonster client monsters =
        let result =
            monsters
            |> List.map (view _1)
            |> ChooseMonster
            |> client

        if List.containsIndex result monsters then monsters.[result]
        else chooseMonster client monsters

    let rec chooseTributes client monsters count old: list<MonsterInstance> =
        match count with
        | 0 -> old
        | count ->
            let resultIndex =
                monsters
                |> map (view _1)
                |> ChooseTributes
                |> client

            if List.containsIndex resultIndex monsters then
                let result = monsters.[resultIndex]
                let withoutCurrent = filter <| Card.MonsterInstance.withoutInstance result <| monsters
                chooseTributes client withoutCurrent <| count - 1 <| result :: old
            else
                chooseTributes client monsters count old


module Zone =
    open Player
    open Side
    open Board

    let freeMonsterZones (player: Player) =
        player.side.monsters
        |> List.indexed
        |> List.filter (view _2 >> Option.isNone)

    let freeMonsterZoneCount = freeMonsterZones >> List.length
    let hasFreeMonsterZones = (>=) << freeMonsterZoneCount
    let hasFreeMonsterZone player = hasFreeMonsterZones player 1

    module Movement =
        let toDeckBottom (card: CardInstance) = over Player.deck (fun deck -> deck @ [ card ])
        let toGraveyard cards = over Player.graveyard (fun deck -> cards @ deck)

module Summon =
    open Card.Card
    open Card.Monster
    open Player
    open Card.CardInstance
    open Card
    open Board
    open Zone
    open Zone.Movement
    open Client
    open Utils

    module Normal =
        let numberOfTributes (monster: Monster) =
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

        let normalSummonable board =
            let hand = board ^. Board.currentPlayerHand
            let monsters = List.choose monster hand

            let isSummonable = view _1 >> isNormalSummonable board

            List.filter isSummonable monsters

        let hasNormalSummonableMonster =
            (normalSummonable
             >> List.length
             >> (<=) 1)

        let canNormalSummon board =
            hasNormalSummonableMonster board && board ^. Board.currentPlayerLastNormalSummon < board ^. Board.turn

        let performNormalSummon client board =
            // Decide what monster to summon
            let summonable = normalSummonable board
            let target = chooseMonster client summonable
            let (_monster, _id) = target

            // Find what monsters to tribute
            let tributteCount = numberOfTributes _monster
            let possibleTributes = board ^. Board.currentPlayerMonsters |>> ((=<<) monster) |> choose id
            let tributes = chooseTributes client possibleTributes tributteCount []

            // helpers to remove the tributes from the board
            let replaceInstance instance =
                let isInstance = List.tryFind (view _2 >> (=) instance.id) tributes |> Option.isSome
                if isInstance then None
                else Some instance

            let replaceTributes = map <| Option.bind replaceInstance

            // Tribute monsters
            let boardWithoutTributes =
                board
                |> over Board.currentPlayerMonsters replaceTributes
                |> over Board.currentPlayer (tributes |>> toCardInstance |> toGraveyard)

            let free = freeMonsterZones <| boardWithoutTributes ^. Board.currentPlayer

            // Choose a zone to summon the monster
            let zone = chooseZone client free
            let turn = boardWithoutTributes ^. Board.turn

            // Instance to actually summon
            let summonedInstance =
                target
                |> toCardInstance
                |> Some

            // Remove card from hand
            let removeTarget = List.filter (fun card -> card.id <> _id)

            // Notify the client a new monster was summoned
            client <| MonsterSummoned(target ^. _1, zone) |> ignore

            // Update the board
            boardWithoutTributes
            |> over Board.currentPlayerHand removeTarget
            |> (Board.currentPlayerMonsters << Lens.indexToLens zone) .-> summonedInstance
            |> Board.currentPlayerLastNormalSummon .-> turn

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

            newBoard
        else
            game <| switchPhases client newBoard <| client
