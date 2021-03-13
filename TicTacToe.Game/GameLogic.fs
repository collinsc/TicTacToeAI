namespace TicTacToe.Game

module GameLogic =
    open System
    open GameTypes
    open ABPruningAI


    let private randomFirstPlayer = 
        if System.Random().Next(0,2) > 0 then 
            XTurn 
        else 
            OTurn


    let getNewGame turn = 
        match turn with
        | Some turn -> Game.Default turn
        | _ -> Game.Default randomFirstPlayer


    let isOver state =
        match state with 
        | State.Turn(_) -> false
        | _ -> true

        
    let getLegalMoves board =
        let statesByIndex = Array2D.mapi(fun r c cell -> (cell, (r,c)))(board)
        statesByIndex 
            |> Seq.cast<CellState*(int*int)>
            |> Seq.filter(fun tup -> fst(tup) = CellState.Empty)
            |> Seq.map(fun tup -> snd(tup))


    let getMovesRemaining board = getLegalMoves board |> Seq.length


    let private computeState (starting: State)(newBoard: CellState[,]) =
        let check3InARow a b c = a <> CellState.Empty && b = a && c = a
        let winCondition = 
            match newBoard with 
            | b when check3InARow b.[0,0] b.[0,1] b.[0,2] -> Some(EndCondition.Row1)
            | b when check3InARow b.[1,0] b.[1,1] b.[1,2] -> Some(EndCondition.Row2)
            | b when check3InARow b.[2,0] b.[2,1] b.[2,2] -> Some(EndCondition.Row3)
            | b when check3InARow b.[0,0] b.[1,0] b.[2,0] -> Some(EndCondition.Column1)
            | b when check3InARow b.[0,1] b.[1,1] b.[2,1] -> Some(EndCondition.Column2)
            | b when check3InARow b.[0,2] b.[1,2] b.[2,2] -> Some(EndCondition.Column3)
            | b when check3InARow b.[0,0] b.[1,1] b.[2,2] -> Some(EndCondition.DiagonalMajor)
            | b when check3InARow b.[0,2] b.[1,1] b.[2,0] -> Some(EndCondition.DiagonalMinor)
            | _ -> None

        let turn = 
            match starting with
            | State.Turn t -> t
            | State.FinalState s -> s.Turn

        match winCondition with 
        | Some condition -> 
            State.FinalState { Turn = turn; EndCondition = condition }
        | None when getMovesRemaining newBoard = 0 -> 
            State.FinalState { Turn = turn; EndCondition = EndCondition.Draw }
        | None ->
            match turn with 
            | XTurn -> State.Turn(OTurn)
            | OTurn -> State.Turn(XTurn)       

    let isEmpty ( board:CellState[,] ) row col = board.[row,col] = CellState.Empty

    let getWinningPlayer state = 
        match state with
        | State.FinalState({EndCondition = EndCondition.Draw}) -> None
        | State.FinalState({Turn=XTurn}) -> Some(OTurn)
        | State.FinalState({Turn=OTurn}) -> Some(XTurn)
        | _ -> None

    let performMove( game:Game )( row:int )( col:int ) =
        if not(isOver game.State) && ( isEmpty game.Board row col ) then 
            let desiredCellState = 
                match game.State  with 
                | State.Turn(XTurn) -> CellState.X
                | State.Turn(OTurn) -> CellState.O
                | _ -> raise(NotImplementedException("???"))

            let newBoard = Array2D.copy(game.Board)
            newBoard.[row,col] <- desiredCellState
            
            let newState = computeState game.State newBoard 
            { State = newState; Board = newBoard }
        else
            game

    // make tree-like object as a sequence
    let private makeLazyABSearchTree game =
               
        let rec makeTree game row col = 
            { new LazyABSearchTree<int*int> with 

                // compute a lazy sequence of moves from our current state
                member this.Children = 
                    if isOver game.State then
                        None // We are a leaf
                    else 
                        Some( // Traversal of legal moves from our state as a lazy sequence
                            seq 
                                { for row, col in getLegalMoves game.Board do
                                    let updated = performMove game row col
                                    yield makeTree updated row col } )

                // Heuristic score at this state
                member this.GetScore searchData = 
                    match game.State with
                    // Staying alive is it's own reward
                    | State.Turn(_) -> searchData.Depth
                    // Tie is still a win for a tic tac toe ai no matter who did it
                    | FinalState{Turn = _; EndCondition = EndCondition.Draw } -> 10
                    | FinalState{Turn = _; EndCondition = _ } -> 
                        if searchData.Maximizing then 
                            // We lost, penalize
                            System.Int32.MinValue +   1 
                        else 
                            // We won! Score wins with less moves higher
                            (50 - searchData.Depth)
                
                // We return the move that got us to this state and score
                member this.Value = (row,col) }

        makeTree game -1 -1
    

    let private makeInitialSearchData game =  ABSearchData<int*int>.Default ( getMovesRemaining game.Board + 5 )

    
    let computeAIMove game =
        // Tic Tac Toe is a solved game so we already know the best opening + response
        let movesRemaining = getLegalMoves game.Board |> Seq.length
        match movesRemaining with
        | 9 -> (0,0)    // best opening is corner square
        | 8 when        // best response is center
            game.Board.[0,0] <> CellState.Empty ||
            game.Board.[2,0] <> CellState.Empty || 
            game.Board.[0,2] <> CellState.Empty ||
            game.Board.[0,2] <> CellState.Empty -> (1,1)
        | _ -> 
            let tree = makeLazyABSearchTree game
            let initialData = makeInitialSearchData game
            let result = doABPruningSearch tree initialData
            printfn $"Selected Move {result.Value}, path {result.PrettyString}"
            result.Value


