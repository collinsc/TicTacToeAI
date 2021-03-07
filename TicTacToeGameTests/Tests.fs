namespace TicTacToeGame.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open TicTacToeGame
open GameTypes
open Game

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestColWins() = 
        seq { 
            (seq { (0,0); (1,1); (1,0); (2,2); (2,0) }, EndCondition.WinCondition(WinCondition.Column1))
            (seq { (0,1); (1,2); (1,1); (2,2); (2,1) }, EndCondition.WinCondition(WinCondition.Column2))
            (seq { (0,2); (1,1); (1,2); (2,1); (2,2) }, EndCondition.WinCondition(WinCondition.Column3))
            } |> this.ExecuteTestSequence

    [<TestMethod>]
    member this.TestRowWins() = 
        seq { 
            (seq { (0,0); (1,1); (0,1); (2,2); (0,2) }, EndCondition.WinCondition(WinCondition.Row1)) 
            (seq { (1,0); (0,1); (1,1); (2,2); (1,2) }, EndCondition.WinCondition(WinCondition.Row2))
            (seq { (2,0); (0,1); (2,1); (1,2); (2,2) }, EndCondition.WinCondition(WinCondition.Row3))
            } |> this.ExecuteTestSequence

    [<TestMethod>]     
    member this.TestDiagWins() = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (0,2); (2,2) }, EndCondition.WinCondition(WinCondition.DiagonoalMajor))
            (seq { (0,2); (0,1); (1,1); (1,2); (2,0) }, EndCondition.WinCondition(WinCondition.DiagonalMinor))
            } |> this.ExecuteTestSequence

    [<TestMethod>]    
    member this.TestDraw() = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (2,2); (1,0); (2,0); (2,1); (1,2); (0,2) }, EndCondition.Draw)
            } |> this.ExecuteTestSequence



    member private this.PlayGame moves endCondition =
        let game = TicTacToeGame()
            
        let turnOrder = 
            game.State 
            |> Seq.unfold 
                (fun state ->
                    match state with 
                    | GameState.Turn(XTurn) -> Some(GameState.Turn(OTurn),GameState.Turn(OTurn))
                    | GameState.Turn(OTurn) -> Some(GameState.Turn(XTurn),GameState.Turn(XTurn))
                    | _ -> None)

        for move, turn in Seq.zip(moves)(turnOrder) do
            assert not(game.IsOver)
            game.TakeTurn(move)
            if not(game.IsOver) then assert (game.State = turn)
        assert game.IsOver
        match game.State with
        | FinalState s -> assert (s.EndCondition = endCondition)
        | _ -> assert false

    member private this.ExecuteTestSequence sequence =
        sequence |> Seq.iter(fun (moves, endResult) -> this.PlayGame moves endResult )
