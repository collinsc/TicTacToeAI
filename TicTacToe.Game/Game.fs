namespace TicTacToe.Game

open System

open GameLogic
open GameTypes
    

// Main interface implementation all mutable state is in this object
type Game(turn:Option<Turn>) =

    let mutable game = GameLogic.getNewGame(turn)
                

    member this.TakeTurn row col = game <- performMove game row col

    member this.TakeAITurn() = 
        let row, col = computeAIMove game
        this.TakeTurn row col
        
    member this.IsOver = isOver game.State

    member this.IsEmpty row col = isEmpty game.Board row col

    member this.State =
        game.State

    member this.Board =
        game.Board

    override this.ToString() =
        let getCellStr (cell:CellState) =
            match cell with
            | CellState.Empty -> " "
            | CellState.X -> "X"
            | CellState.O -> "O"
            | _ -> raise(NotImplementedException("???"))
    
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
                | State.FinalState({Turn = t; EndCondition = e}) ->  
                    match e with 
                    | EndCondition.Draw -> sprintf "%s forced draw" (getPlayer t)
                    | _ -> sprintf "%s won by %O" (getPlayer t) e 
                | State.Turn(t)-> sprintf "It's %s's turn" (getPlayer t)
    
        let buildStrings = Array2D.mapi(fun r c cell -> getDecoratedCell(r)(c)(cell))
    
        let strSeq = buildStrings(this.Board) |> Seq.cast<string>
    
        String.Concat(seq { 
            sprintf "%s\n" (getStateStr this.State)
            yield! strSeq})



    

    
        
    
    