namespace TicTacToe.Game


module GameLogic =
    open System
    open GameTypes
    open ABPruningAI


    let private randomFirstPlayer() = 
        if System.Random().Next(0, 2) > 0 then 
            Player.X 
        else 
            Player.O


    let isEmpty cell = cell = CellState.Empty


    let flipTurn turn = 
        match turn with 
        | Player.X -> Player.O
        | Player.O -> Player.X
        | _ -> raise(NotImplementedException("???"))



    let isEmptyCell ( board:CellState[,] ) row col = isEmpty board.[row,col]

    let getNewGame turn = 
        match turn with
        | Some turn -> Game.Default turn
        | _ -> Game.Default (randomFirstPlayer())


    let isOver state =
        match state with 
        | Turn(_) -> false
        | _ -> true

        
    let getLegalMoves board =
        let getStateByIndex r c cell  = (cell, (r, c))
        let isEmpty cellIndex = isEmpty(fst(cellIndex))
        let getIndex cellIndex = snd(cellIndex)
        let statesByIndex = Array2D.mapi getStateByIndex board
        let nonEmptyIndexes = 
            statesByIndex 
            |> Seq.cast<CellState*(int*int)> 
            |> Seq.filter(isEmpty) 
            |> Seq.map(getIndex)
        nonEmptyIndexes


    let getMovesRemaining board = getLegalMoves board |> Seq.length


    let private computeState (starting:State)(newBoard:CellState[,]) =
        let check3InARow (a:CellState) b c = not(isEmpty(a)) && b = a && c = a
        let winCondition = 
            match newBoard with 
            | b when check3InARow b.[0, 0] b.[0, 1] b.[0, 2] -> Some(EndCondition.Row1)
            | b when check3InARow b.[1, 0] b.[1, 1] b.[1, 2] -> Some(EndCondition.Row2)
            | b when check3InARow b.[2, 0] b.[2, 1] b.[2, 2] -> Some(EndCondition.Row3)
            | b when check3InARow b.[0, 0] b.[1, 0] b.[2, 0] -> Some(EndCondition.Column1)
            | b when check3InARow b.[0, 1] b.[1, 1] b.[2, 1] -> Some(EndCondition.Column2)
            | b when check3InARow b.[0, 2] b.[1, 2] b.[2, 2] -> Some(EndCondition.Column3)
            | b when check3InARow b.[0, 0] b.[1, 1] b.[2, 2] -> Some(EndCondition.DiagonalMajor)
            | b when check3InARow b.[0, 2] b.[1, 1] b.[2, 0] -> Some(EndCondition.DiagonalMinor)
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
        | None -> State.Turn(flipTurn turn)


    let getWinningPlayer state = 
        match state with
        | State.FinalState({EndCondition = EndCondition.Draw}) -> None
        | State.FinalState({Turn=Player.X}) -> Some(Player.X)
        | State.FinalState({Turn=Player.O}) -> Some(Player.O)
        | _ -> None

    let performMove( game:Game )( row:int )( col:int ) =
        if not(isOver game.State) && ( isEmptyCell game.Board row col ) then 
            let desiredCellState = 
                match game.State  with 
                | Turn(t) -> flipTurn t
                | _ -> raise(NotImplementedException("???"))

            let newBoard = Array2D.copy(game.Board)
            newBoard.[row,col] <- CellState.Player(desiredCellState)
            
            let newState = computeState game.State newBoard 
            { State = newState; Board = newBoard }
        else
            game

    let aiGame =
        {   new IPlayableGame<Game,int*int> with
                member this.ComputeState game move = 
                    performMove game (fst move) (snd move)
                member this.LegalMoves game = 
                    match getLegalMoves game.Board with 
                    | a when (not (isOver game.State)) &&  Seq.tryHead a <> None -> Some(a)
                    | _ -> None
                member this.Score root = 
                    match root.State.State  with
                    | State.Turn(_) -> 0
                    // Tie is still a win for a tic tac toe ai no matter who did it
                    | FinalState{Turn = _; EndCondition = EndCondition.Draw } -> 10
                    | FinalState{Turn = _; EndCondition = _ } -> 
                        if root.Maximizing then 
                            // We lost, penalize
                            System.Int32.MinValue +   1 
                        else 
                            // We won! Score wins with less moves higher
                            System.Int32.MaxValue - root.Depth }
    
    

    
    let computeAIMove isDeterministic game =

        let corners = [| (0, 0); (0, 2); (2, 0); (2, 2) |]
        
        let computeAIOpeningMove() = 
            corners.[System.Random().Next(0, 4)]
        
        let isCornerFilled() =
            let checkCorner(row,col) = not(isEmptyCell game.Board row col)
            match Array.tryFind checkCorner corners with
            | Some(_) -> true
            | _ -> false
            
        
        // Tic Tac Toe is a solved game so we already know the best opening + response
        let movesRemaining = getLegalMoves game.Board |> Seq.length
        match movesRemaining with
        | 9 -> 
            if isDeterministic then // best opening is corner square
                (0, 0)
            else 
                computeAIOpeningMove()
        | 8 when isCornerFilled()  -> (1, 1) // best response is center
        | _ -> 
            let initialData = ABSearchData<Game,int*int>.Default ( getMovesRemaining game.Board ) game
            let result = ABPruningAI.ComputeSearch initialData aiGame
            result.NextMove.Value
