namespace TicTacToe.Game

open GameLogic
open GameTypes


// Mutable object holds game state, major use cases and basic display
type Instance(turn:Option<Turn>) =
    let mutable game = GameLogic.getNewGame(turn)

    member this.IsEmpty(row, col) = 
        game.Board.[row, col] = Empty

    member this.TakeTurn(row:int, col:int) =
        game <- performMove(game)(row, col)

    member this.TakeAITurn() = 
        let move = computeAIMove(game)
        this.TakeTurn(move)
        
    member this.IsOver =
        isOver game

    member this.State =
        game.State

    member this.Board =
        game.Board
        

            
        
    
    