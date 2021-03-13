namespace TicTacToe.Game
open System
open GameTypes
open GameLogic




// All Functional -> OO interface interop goes here
type ServiceInterop (turn:Option<Turn>) = 
    let game = Game(turn)

    interface IGameService with
        member this.WinningPlayer: Turn = 
            match getWinningPlayer game.State with
            | Some(turn) -> turn
            | _ -> Unchecked.defaultof<_>

        member this.TakeTurn (row: int) (col: int)= game.TakeTurn row col

        member this.TakeAITurn() = game.TakeAITurn()

        member this.IsLegal(row: int) (col: int): bool = game.IsEmpty row col
        
        member this.ActivePlayer =
            match game.State with
            | State.Turn t -> t
            | _ -> Unchecked.defaultof<_>


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
        | _ as turn -> ( ServiceInterop(Some(turn)) :> IGameService )

        