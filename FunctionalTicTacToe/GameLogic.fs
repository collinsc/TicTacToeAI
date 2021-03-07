namespace TicTacToeGame

module GameLogic =
    open System
    open GameTypes
    open ABPruning


    let private randomFirstPlayer = 
        if System.Random().Next(0,2) > 0 then 
            XTurn 
        else 
            OTurn


    let getNewGame = Game.Default randomFirstPlayer


    let isOver game =
        match game.State with 
        | GameState.Turn(_) -> false
        | _ -> true

        
    let getLegalMoves board =
        
        let cells = Array2D.mapi(fun r c cell -> (cell, (r,c)))(board)
        cells 
            |> Seq.cast<CellState*(int*int)>
            |> Seq.filter(fun tup -> fst(tup) = Empty)
            |> Seq.map(fun tup -> snd(tup))


    let getMovesRemaining board = getLegalMoves board |> Seq.length


    let private computeState (starting: GameState)(newBoard: CellState[,]) =
        let check3InARow a b c = a <> Empty && b = a && c = a
        let winCondition = 
            match newBoard with 
            | b when check3InARow b.[0,0] b.[0,1] b.[0,2] -> Some(WinCondition.Row1)
            | b when check3InARow b.[1,0] b.[1,1] b.[1,2] -> Some(WinCondition.Row2)
            | b when check3InARow b.[2,0] b.[2,1] b.[2,2] -> Some(WinCondition.Row3)
            | b when check3InARow b.[0,0] b.[1,0] b.[2,0] -> Some(WinCondition.Column1)
            | b when check3InARow b.[0,1] b.[1,1] b.[2,1] -> Some(WinCondition.Column2)
            | b when check3InARow b.[0,2] b.[1,2] b.[2,2] -> Some(WinCondition.Column3)
            | b when check3InARow b.[0,0] b.[1,1] b.[2,2] -> Some(WinCondition.DiagonoalMajor)
            | b when check3InARow b.[0,2] b.[1,1] b.[2,0] -> Some(WinCondition.DiagonalMinor)
            | _ -> None

        let turn = 
            match starting with
            | GameState.Turn t -> t
            | GameState.FinalState s -> s.Turn

        match winCondition with 
        | Some condition -> 
            GameState.FinalState { Turn = turn; EndCondition = (EndCondition.WinCondition condition) }
        | None when getMovesRemaining newBoard = 0 -> 
            GameState.FinalState { Turn = turn; EndCondition = Draw }
        | None ->
            match turn with 
            | XTurn -> GameState.Turn(OTurn)
            | OTurn -> GameState.Turn(XTurn)       


    let performMove(game:Game)(move:int*int) =
        let row, col = move
        if not(isOver game) && (game.Board.[row,col] = Empty) then 
            let desiredCellState = 
                match game.State  with 
                | GameState.Turn(XTurn) -> X
                | GameState.Turn(OTurn) -> O
                | _ -> raise(NotImplementedException("???"))

            let newBoard = Array2D.copy(game.Board)
            newBoard.[row,col] <- desiredCellState
            
            let newState = computeState game.State newBoard 
            { State = newState; Board = newBoard }
        else
            game


    let private makeABSearchTree game =
        let rec makeTree game move = 
            { new LazyABSearchTree<int*int> with 

                member this.Children = 
                    if isOver game then
                        None // We are a leaf
                    else 
                        Some( // Traversal of legal moves from our state as a lazy sequence
                            seq 
                                { for move in getLegalMoves game.Board do
                                    let updated = performMove game move
                                    yield makeTree updated move } )

                member this.GetScore searchData = 
                    match game.State with
                    | GameState.Turn(_) ->  0
                    // Tie is still a win for a tic tac toe ai no matter who did it
                    | FinalState{Turn = _; EndCondition = EndCondition.Draw } -> 10
                    | FinalState{Turn = _; EndCondition = EndCondition.WinCondition(_) } -> 
                        // If we end the game and it's the ai's turn the ai lost
                        let scoreScale = if searchData.Maximizing then -1 else 1 
                        // Score wins with less moves higher
                        scoreScale * 50 - searchData.Depth

                member this.Value = move }
        makeTree game (-1,-1)
    

    let private makeInitialSearchData game =  ABSearchData<int*int>.Default(getMovesRemaining game.Board)

    
    let computeAIMove game =
        // Tic Tac Toe is a solved game so lets save some compute time on the 1st & 2nd moves
        let movesRemaining = getLegalMoves game.Board |> Seq.length
        match movesRemaining with
        | 9 -> (0,0)    // best opening is corner square
        | 8 when        // best response is center
            game.Board.[0,0] <> Empty ||
            game.Board.[2,0] <> Empty || 
            game.Board.[0,2] <> Empty ||
            game.Board.[0,2] <> Empty -> (1,1)
        | _ -> 
            let tree = makeABSearchTree game
            let initialData = makeInitialSearchData game
            (doABPruningSearch tree initialData).Value