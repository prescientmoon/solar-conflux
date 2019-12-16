module Main =
    open FSharpPlus.Lens
    open Board.Player
    open Board.Board
    open Board.Game
    open Board.Client
    open Card.Card
    open Card.MonsterTypes
    open Card.BaseCard

    let printState state = 
        match state with
        | Lost reason -> sprintf "lost because did: %s" reason
        | Won reason -> sprintf "won because opponent did: %s" reason
        | InGame -> "still playing"

    [<EntryPoint>]
    let main _ =
        let sampleCardTemplate = 
            Monster ({ name= "sampleCard" 
                       text="something"
                       effects = []}
                    ,{ attack = 0
                       defense = 0 
                       level = 3 
                       attribute = Fire
                       race = Warrior })

        let tributeCardTemplate = 
            Monster ({ name= "sampleCard2" 
                       text="something"
                       effects = []}
                    ,{ attack = 3000
                       defense = 2500 
                       level = 7 
                       attribute = Fire
                       race = Warrior })

        let (sampleCard1, board1) = instantiate emptyBoard sampleCardTemplate
        let (sampleCard2, board2) = instantiate board1 sampleCardTemplate
        let (sampleCard3, board3) = instantiate board2 sampleCardTemplate
        let (sampleCard4, board4) = instantiate board3 sampleCardTemplate
        let (tributeCard, board5) = instantiate board4 tributeCardTemplate        

        let board6 = over Board.firstPlayer <| toDeckBottom sampleCard1 <| board5
        let board7 = over Board.firstPlayer <| toDeckBottom sampleCard2 <| board6
        let board8 = over Board.secondPlayer <| toDeckBottom sampleCard3 <| board7
        let board9 = over Board.secondPlayer <| toDeckBottom sampleCard4 <| board8
        let board10 = over Board.firstPlayer <| toDeckBottom tributeCard <| board9

        let client action =
            match action with
            | StateChanged (s1, s2) -> 
                printfn "Player 1: %s" <| printState s1
                printfn "Player 2: %s" <| printState s2
                0
            | NewPhase phase -> 
                printfn "New phse: %A" phase
                0
            | ChooseZone free -> 
                printfn "What Zone do wou want to use? %A" free
                System.Console.ReadLine() |> int
            | ChooseMonster monsters ->
                printfn "What monster do you want to choose? %A" <| List.map (fun (_base, details) -> _base.name) monsters
                System.Console.ReadLine() |> int
             | ChooseTributes monsters ->
                printfn "What monster do you want to tribute? %A" <| List.map (fun (_base, details) -> _base.name) monsters
                System.Console.ReadLine() |> int
            | MonsterSummoned (card, zone) -> 
                printfn "Monster %A was summoned in zone %i" card zone
                0
            | _ -> 
                printfn "Something unkown happened" 
                0

        let finalBoard = game board10 client

        printfn "The final baord was: %A" finalBoard

        0 // return integer code
