module Main =
    open FSharpPlus.Lens
    open Board.Board
    open Board.Game
    open Card

    [<EntryPoint>]
    let main _ =
        let sampleCard = Card.Spell ({name= "sampleCard"; text="something"; effects = []}, {spellType = Card.ContinuosSpell})
        let board = over Board.currentPlayer <| toDeckBottom sampleCard <| emptyBoard

        let consoleClient command = 
            match command with
            | Log message -> 
                printfn "%s" message
                NoResult
            // | _ -> NoResult

        let parseAction _ = 
            printf "What action do you want to do?"

            let stringified = System.Console.ReadLine()

            match stringified with
            | "draw" -> InitialDraw
            | "pass" -> Pass
            | _ -> failwith "unknown command"
 


        let rec loop board = 
            let action = parseAction

            let (newBoard, success) = processAction consoleClient board <| board^. Board.currentPlayer <| action()

            if success then
                printfn "%A" newBoard
            printfn "%b" success

            loop newBoard

        loop board
        0
