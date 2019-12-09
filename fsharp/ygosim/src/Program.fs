module Main =
    open FSharpPlus.Lens
    open Board.Board
    open Board.Game
    open Card

    [<EntryPoint>]
    let main _ =
        let board = emptyBoard
        let sampleCard = Card.Spell ({name= "sampleCard"; text="something"; effects = []}, {spellType = Card.ContinuosSpell})

        let secondBoard = over Board.currentPlayer <| toDeckBottom sampleCard <| board

        printfn "%A" secondBoard

        let thirdBoard = doTurn secondBoard

        printfn "%A" thirdBoard

        let lastBoard = List.fold (fun b _ -> doTurn b) thirdBoard [0..5]

        printf "%A" lastBoard

        0
