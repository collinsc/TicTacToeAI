namespace TicTacToe.Game
module GameTypes =

    type CellState = 
        | Empty = 0
        | X = 1
        | O = 2
    

    

    type Turn = 
        | XTurn
        | OTurn
    

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
        { Turn : Turn
          EndCondition : EndCondition }

    
    type GameState =
        | Turn of Turn
        | FinalState of FinalState


    type Game =
        { Board : CellState[,]
          State : GameState }

        static member Default turn =
            { Board= Array2D.create 3 3 CellState.Empty
              State = GameState.Turn(turn)}