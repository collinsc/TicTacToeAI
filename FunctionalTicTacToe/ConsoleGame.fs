namespace TicTacToe.BusinessLogic.ConsoleProgram
module ConsoleGame = 
    open System
    open TicTacToe.BusinessLogic
    open GameTypes

    // processes input & feeds to TicTacToe object
    type ConsoleGame()  =
        let game = Game.TicTacToeGame()
        do 
            printfn "%s" game.StringOutput
            // first move is randomized so AI might go first, currently O is ai
            if not(game.State = GameState.Turn(OTurn)) then
                game.TakeAITurn()
                printfn "%s" game.StringOutput

        member this.InputPrompt = "Enter an index:"
        
        member this.ProcessInput (input:string) = 
            let tokens = input.Trim().Split([|' ';','|]) 

            let quitValue = 
                tokens 
                |> Seq.map(fun s -> s.ToLower())
                |> Seq.tryPick(fun s ->
                    match s with 
                    | "quit" -> Some()
                    | _ -> None)

            if quitValue.IsSome then 
                printfn "quit"
                false
            else
                let indexValue = 
                    let indexTokens =
                        tokens 
                        |> Seq.filter(fun s -> s <> null && s.Length > 0)
                        |> Seq.map(fun s -> 
                            let mutable i = -1
                            if Int32.TryParse(s, &i) then i else -1)
                        |> Seq.filter(fun i -> i > 0 && i < 3)
                        |> Seq.toArray
                    match indexTokens with 
                    | [|r; c|]-> Some(r,c)
                    | _ -> None

                if indexValue.IsSome then
                    if game.IsEmpty indexValue.Value then
                        game.TakeTurn(indexValue.Value)
                        printfn "%s" game.StringOutput
                        if not(game.IsOver) then
                            game.TakeAITurn()
                            printfn "%s" game.StringOutput
                        not(game.IsOver)
                    else
                        printfn "Cell %O is not empty" indexValue.Value
                        true
                else
                    printfn "Could not parse input %s, expects pair of indexes in row-major order or quit" input
                    true






        





