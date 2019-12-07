module Main =
    open Board
    open Card
    open Game

    [<EntryPoint>]
    let main argv =
        let board = Board.emptyBoard
        let sampleCard = Spell ({name= "sampleCard"; text="something"}, {spellType = Card.ContinuosSpell})

        let (first, second) = board.players
        let secondBoard = withCurrentPlayer <| Game.toDeckBottom sampleCard <| board

        printfn "%A" secondBoard

        let thirdBoard = doTurn secondBoard

        printfn "%A" thirdBoard

        0
