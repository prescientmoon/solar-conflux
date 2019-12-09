module Main =
    open Board
    open Card
    open Game

    [<EntryPoint>]
    let main _ =
        let board = Board.emptyBoard
        let sampleCard = Card.Spell ({name= "sampleCard"; text="something"; effects = []}, {spellType = Card.ContinuosSpell})

        let secondBoard = withCurrentPlayer <| Game.toDeckBottom sampleCard <| board

        printfn "%A" secondBoard

        let thirdBoard = doTurn secondBoard

        printfn "%A" thirdBoard

        let lastBoard = List.fold (fun b _ -> doTurn b) thirdBoard [0..5]

        printf "%A" lastBoard

        0
