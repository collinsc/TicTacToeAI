namespace TicTacToe.ConsoleGame


module ConsoleGame = 

    open System
    open TicTacToe.Game
    open GameTypes
    open MutableState

    // processes input & feeds to TicTacToe object
    type ConsoleGame() as this = 
        let game = Game(Some(XTurn))
        do 
            printfn "%s" this.BuildGameString
            // first move is randomized so AI might go first, currently O is ai
            if game.State = State.Turn(OTurn) then
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
                        printfn "%s" this.BuildGameString
                        if not(game.IsOver) then
                            game.TakeAITurn()
                            printfn "%s" this.BuildGameString
                            not game.IsOver
                        else 
                            false
                        
                    else
                        printfn "Cell %O is not empty" indexValue.Value
                        true
                else
                    printfn "Could not parse input %s, expects pair of indexes in row-major order or quit" input
                    true


        member this.BuildGameString = 
        
            let getCellStr (cell:CellState) =
                match cell with
                | CellState.Empty -> " "
                | CellState.X -> "X"
                | CellState.O -> "O"
                | _ -> raise(NotImplementedException("???"))
        
            let getDecoratedCell row col cell =
                match (row, col) with
                | (0, 2) 
                | (1, 2) ->   getCellStr(cell) + "\n-----\n" 
                | (2, 2) -> getCellStr(cell)
                | (1, _) 
                | (2, _) 
                | (0, _) -> getCellStr(cell) + "|"
                | _ -> raise(NotImplementedException("???"))
        
            let getStateStr state = 
                let getPlayer p =
                    match p with 
                    | XTurn -> "X"
                    | OTurn -> "O"
                match state with
                    | State.FinalState({Turn = t; EndCondition = e}) ->
                        match e with 
                        | EndCondition.Draw -> sprintf "%s forced draw" (getPlayer t)
                        | _ -> sprintf "%s won by %O" (getPlayer t) e 
                    | State.Turn(t)-> sprintf "It's %s's turn" (getPlayer t)
        
            let buildStrings = Array2D.mapi(fun r c cell -> getDecoratedCell(r)(c)(cell))
        
            let strSeq = buildStrings(game.Board) |> Seq.cast<string>
        
            String.Concat(seq { 
                sprintf "%s\n" (getStateStr game.State)
                yield! strSeq})
