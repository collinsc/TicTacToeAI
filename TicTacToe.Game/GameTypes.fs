namespace TicTacToe.Game
module GameTypes =
    type CellState = 
        | Empty
        | X
        | O
    
    type WinCondition =
        | Row1
        | Row2
        | Row3
        | Column1
        | Column2
        | Column3
        | DiagonalMajor
        | DiagonalMinor
    
    type Turn = 
        | XTurn
        | OTurn
    
    type EndCondition = 
        | Draw
        | WinCondition of WinCondition
    
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
            { Board= Array2D.create 3 3 Empty
              State = GameState.Turn(turn)}