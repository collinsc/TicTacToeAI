namespace TicTacToe.Game

open System

open GameLogic
open GameTypes
    

// Mutable object holds game state, major use cases and basic display
// Contains get accessors for easy C# interop
type Instance(turn:Option<Turn>) =

    let mutable game = GameLogic.getNewGame(turn)

    member this.IsEmpty(row, col) = 
        game.Board.[row, col] = CellState.Empty

    member this.TakeTurn(row:int, col:int) =
        game <- performMove(game)(row, col)

    member this.TakeAITurn() = 
        let move = computeAIMove(game)
        this.TakeTurn(move)
        
    member this.IsOver =
        isOver game

    member this.ActivePlayer =
        match this.State with
        | GameState.Turn t -> t
        | _ -> Unchecked.defaultof<_>

    member this.EndCondition = 
        match this.State with 
        | GameState.FinalState t -> t
        | _ -> Unchecked.defaultof<_>

    member this.State =
        game.State

    member this.Board =
        game.Board
        

            
        
    
    