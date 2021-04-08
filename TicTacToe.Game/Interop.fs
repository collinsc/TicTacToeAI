namespace TicTacToe.Game


module Interfaces =
    open System
    open GameTypes
    open GameLogic
    open MutableState
    

    // All Functional -> OO interface interop goes here
    type ServiceInterop (turn: Player Option) = 
        let game = Game(turn)
    
        interface IGameService with
            member this.WinningPlayer: Nullable<Player> = 
                match getWinningPlayer game.State with
                | Some(turn) -> Nullable(turn)
                | _ -> System.Nullable()
    
            member this.TakeTurn (row:int) (col:int)= game.TakeTurn row col
    
            member this.TakeAITurn(difficulty:Difficulty) = game.TakeAITurn(difficulty)
    
            member this.IsLegal(row:int) (col:int) : bool = game.IsEmpty row col
            
            member this.ActivePlayer =
                match game.State with
                | State.Turn t -> Nullable(t)
                | _ -> System.Nullable()
    
    
            member this.EndCondition = 
                match game.State with 
                | State.FinalState t -> t
                | _ -> Unchecked.defaultof<_>
    
            member this.IsOver = game.IsOver
    
            member this.GetCellState row col = game.Board.[row,col]
    
    
    
    type ServiceFactory =
        static member CreateInstanceRandomPlayer() : IGameService = 
            (ServiceInterop None :> IGameService)
    
        static member CreateInstanceFirstPlayer turn : IGameService = 
            match turn with 
            | turn -> ( ServiceInterop(Some(turn)) :> IGameService )
