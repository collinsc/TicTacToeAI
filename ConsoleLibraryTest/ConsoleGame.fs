namespace TicTacToeGame.ConsoleGame
module ConsoleGame = 
    open TicTacToeGame
    open GameTypes
    open System

    // processes input & feeds to TicTacToe object
    type ConsoleGame() as this = 
        let game = Game.TicTacToeGame()
        do 
            printfn "%s" this.BuildGameString
            // first move is randomized so AI might go first, currently O is ai
            if not(game.State = GameState.Turn(OTurn)) then
                game.TakeAITurn()
                printfn "%s" this.BuildGameString

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
                        |> Seq.filter(fun i -> i >= 0 && i < 3)
                        |> Seq.toArray
                    match indexTokens with 
                    | [|r; c|]-> Some(r,c)
                    | _ -> None

                if indexValue.IsSome then
                    if game.IsEmpty indexValue.Value then
                        game.TakeTurn(indexValue.Value)
                        printfn "%s" this.BuildGameString
                        if not(game.IsOver) then
                            game.TakeAITurn()
                            printfn "%s" this.BuildGameString
                            not(game.IsOver)
                        else
                            not(game.IsOver)
                    else
                        printfn "Cell %O is not empty" indexValue.Value
                        true
                else
                    printfn "Could not parse input %s, expects pair of indexes in row-major order or quit" input
                    true


        member this.BuildGameString = 
        
            let getCellStr cell =
                match cell with
                | Empty -> " "
                | X -> "X"
                | O -> "O"
        
            let getDecoratedCell row col cell =
                match (row,col) with
                | (0,2) | (1,2) ->   getCellStr(cell) + "\n-----\n" 
                | (2,2) -> getCellStr(cell)
                | (1,_) | (2,_) | (0,_) -> getCellStr(cell) + "|"
                | _ -> raise(NotImplementedException("???"))
        
            let getStateStr state = 
                let getPlayer p =
                    match p with 
                    | XTurn -> "X"
                    | OTurn -> "O"
                match state with
                    | GameState.FinalState({Turn = t; EndCondition = e}) ->  
                        match e with 
                        | Draw -> sprintf "%s forced draw" (getPlayer t)
                        | _ -> sprintf "%s won by %O" (getPlayer t) e 
                    | GameState.Turn(t)-> sprintf "It's %s's turn" (getPlayer t)
        
            let buildStrings = Array2D.mapi(fun r c cell -> getDecoratedCell(r)(c)(cell))
        
            let strSeq = buildStrings(game.Board) |> Seq.cast<string>
        
            String.Concat(seq { 
                sprintf "%s\n" (getStateStr game.State)
                yield! strSeq})
    





        





