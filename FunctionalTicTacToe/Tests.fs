namespace TicTacToe.BusinessLogic.Tests
module Tests =
    open TicTacToe.BusinessLogic
    open Game
    open GameTypes


    let private playGame moves endCondition =
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
            assert (game.State = turn)
            game.TakeTurn(move)
        assert game.IsOver
        match game.State with
        | FinalState s -> assert (s.EndCondition = endCondition)
        | _ -> assert false


    let private executeTestSequence sequence =
        sequence |> Seq.iter(fun (moves, endResult) -> playGame moves endResult )

    let testColWin = 
        seq { 
            (seq { (0,0); (1,1); (1,0); (2,2); (2,0) }, EndCondition.WinCondition(WinCondition.Column1))
            (seq { (0,1); (1,2); (1,1); (2,2); (2,1) }, EndCondition.WinCondition(WinCondition.Column2))
            (seq { (0,2); (1,1); (1,2); (2,1); (2,2) }, EndCondition.WinCondition(WinCondition.Column3))
            } |> executeTestSequence


    let testRowWin = 
        seq { 
            (seq { (0,0); (1,1); (0,1); (2,2); (0,2) }, EndCondition.WinCondition(WinCondition.Row1)) 
            (seq { (1,0); (0,1); (1,1); (2,2); (1,2) }, EndCondition.WinCondition(WinCondition.Row2))
            (seq { (2,0); (0,1); (2,1); (1,2); (2,2) }, EndCondition.WinCondition(WinCondition.Row3))
            } |> executeTestSequence

            
    let testDiagWin = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (0,2); (2,2) }, EndCondition.WinCondition(WinCondition.DiagonoalMajor))
            (seq { (0,2); (0,1); (1,1); (1,2); (2,0) }, EndCondition.WinCondition(WinCondition.DiagonalMinor))
            } |> executeTestSequence


    let testDraw = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (2,2); (1,0); (2,0); (2,1); (1,2); (0,2) }, EndCondition.Draw)
            } |> executeTestSequence

    let runAll =
        testRowWin
        testColWin
        testDiagWin
        testDraw


