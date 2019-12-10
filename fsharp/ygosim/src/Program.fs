module Main =
    open FSharpPlus.Lens
    open Board.Board
    open Board.Game
    open Card

    [<EntryPoint>]
    let main _ =
        let sampleCard = Card.Spell ({name= "sampleCard"; text="something"; effects = []}, {spellType = Card.ContinuosSpell})
        let board = over Board.currentPlayer <| toDeckBottom sampleCard <| emptyBoard

        let client action =
            match action with
            | StateChanged newState -> printfn "The new state you got is: %A" newState
            | NewPhase phase -> printfn "New phse: %A" phase
            | _ -> printfn "Something unkown happened"

            0

        game board client
        0
