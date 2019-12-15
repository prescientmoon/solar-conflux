module Main =
    open FSharpPlus.Lens
    open Board.Player
    open Board.Board
    open Board.Game
    open Board.Client
    open Card.Card

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


        let (sampleCard, initialBoard) = instantiate emptyBoard sampleCardTemplate
                 
        let board = over Board.firstPlayer <| toDeckBottom sampleCard <| initialBoard

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
                let i = System.Console.ReadLine() |> int

                free.[i]
            | ChooseMonster monsters ->
                printfn "What monster do you want to choose? %A" <| List.map (fun (_base, details) -> _base.name) monsters
                let i = System.Console.ReadLine() |> int

                i
            | MonsterSummoned (card, zone) -> 
                printfn "Monster %A was summoned in zone %i" card zone
                0
            | _ -> 
                printfn "Something unkown happened" 
                0

        let finalBoard = game board client

        printfn "The final baord was: %A" finalBoard

        0 // return integer code
