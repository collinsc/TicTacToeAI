namespace TicTacToe.Game
module GameTypes =


    type Player = 
        | X = 0
        | O = 1

    type CellState =
        | Player of Player
        | Empty
    

    type EndCondition = 
        | Row1 = 0
        | Row2 = 1
        | Row3 = 2
        | Column1 = 3
        | Column2 = 4
        | Column3 = 5
        | DiagonalMajor = 6
        | DiagonalMinor = 7
        | Draw = 8
    

    type FinalState = 
        { Turn:Player
          EndCondition:EndCondition }

    
    type State =
        | Turn of Player
        | FinalState of FinalState



    type Game =
        { Board:CellState[,]
          State:State }

        static member Default turn =
            { Board= Array2D.create 3 3 CellState.Empty
              State = State.Turn(turn) }


    type IGameService = 
        abstract member TakeTurn : row:int -> col:int -> unit
        abstract member TakeAITurn : unit -> unit
        abstract member IsLegal : row:int -> col:int -> bool
        abstract member GetCellState : row:int -> col:int -> CellState
        abstract member IsOver : bool
        abstract member ActivePlayer : System.Nullable<Player>
        abstract member EndCondition : FinalState
        abstract member WinningPlayer : System.Nullable<Player>
