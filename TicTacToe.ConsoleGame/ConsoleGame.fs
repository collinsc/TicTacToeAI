namespace TicTacToe.ConsoleGame


module ConsoleGame = 

    open System
    open TicTacToe.Game
    open GameTypes
    open MutableState

    // processes input & feeds to TicTacToe object
    type ConsoleGame()  = 
        let game = Game(Some(Player.X))
        do 
            printfn "%s" (game.ToString())            // first move is randomized so AI might go first, currently O is ai
            if game.State = State.Turn(Player.O) then
                game.TakeAITurn Difficulty.Impossible
                printfn "%s" (game.ToString())

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
                        |> Seq.filter(fun s -> not(isNull s) && s.Length > 0)
                        |> Seq.map(fun s -> 
                            let mutable i = -1
                            if Int32.TryParse(s, &i) then i else -1)
                        |> Seq.filter(fun i -> i >= 0 && i < 3)
                        |> Seq.toArray
                    match indexTokens with 
                    | [|r; c|]-> Some(r, c)
                    | _ -> None

                if indexValue.IsSome then
                    let row, col = indexValue.Value
                    if game.IsEmpty row col then
                        game.TakeTurn row col
                        printfn "%s" (game.ToString())
                        if not(game.IsOver) then
                            game.TakeAITurn Difficulty.Impossible
                            printfn "%s" (game.ToString())
                            not game.IsOver
                        else 
                            false
                        
                    else
                        printfn "Cell %O is not empty" indexValue.Value
                        true
                else
                    printfn "Could not parse input %s, expects pair of indexes in row-major order or quit" input
                    true
