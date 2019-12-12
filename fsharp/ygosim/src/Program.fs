module Main =
    open FSharpPlus.Lens
    open Board.Player
    open Board.Board
    open Board.Game
    open Card

    let printState state = 
        match state with
        | Lost reason -> sprintf "lost because did: %s" reason
        | Won reason -> sprintf "won because opponent did: %s" reason
        | InGame -> "still playing"

    [<EntryPoint>]
    let main _ =
        let sampleCard = Card.Spell ({name= "sampleCard"; text="something"; effects = []}, {spellType = Card.ContinuosSpell})
        let board = over Board.currentPlayer <| toDeckBottom sampleCard <| emptyBoard

        let client action =
            match action with
            | StateChanged (s1, s2) -> 
                printfn "Player 1: %s" <| printState s1
                printfn "Player 2: %s" <| printState s2
            | NewPhase phase -> printfn "New phse: %A" phase
            | _ -> printfn "Something unkown happened"

            0

        game board client


        0 // return integer code
