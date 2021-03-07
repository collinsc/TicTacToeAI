namespace TicTacToe.BusinessLogic
module Game = 
    open GameLogic
    open GameTypes
    open System
    
    // Mutable object holds game state, major use cases and basic display
    type TicTacToeGame() =
        let mutable game = GameLogic.getNewGame

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
        
        member this.StringOutput = 
            buildGameString game

            
        
    
    