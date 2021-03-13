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



    

    
        
    
    