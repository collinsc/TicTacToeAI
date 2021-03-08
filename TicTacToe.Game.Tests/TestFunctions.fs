namespace TicTacToe.Game.Tests

module TestFunctions = 
    open Microsoft.VisualStudio.TestTools.UnitTesting
    open TicTacToe.Game.GameTypes
    open TicTacToe.Game

    let playGame doAi startingTurn moves endCondition =
        let game = Instance(startingTurn)
            
        let turnOrder = 
            game.State 
            |> Seq.unfold 
                (fun state ->
                    match state with 
                    | _ when doAi -> Some(GameState.Turn(startingTurn.Value), GameState.Turn(startingTurn.Value))
                    | GameState.Turn(XTurn) -> Some(GameState.Turn(OTurn),GameState.Turn(OTurn))
                    | GameState.Turn(OTurn) -> Some(GameState.Turn(XTurn),GameState.Turn(XTurn))
                    | _ -> None)

        for move, turn in Seq.zip(moves)(turnOrder) do
            Assert.IsFalse game.IsOver
            match turn with 
            | GameState.Turn(OTurn) when doAi -> 
                game.TakeAITurn()
                if not(game.IsOver) then game.TakeTurn(move)
            | GameState.Turn(XTurn) when doAi -> 
                game.TakeTurn(move)
                if not (game.IsOver) then game.TakeAITurn()
            | _ -> game.TakeTurn(move)
            if not(game.IsOver) then Assert.AreEqual(turn, game.State) 
        // fencepost
        if (doAi 
            && startingTurn.IsSome 
            && startingTurn.Value = OTurn 
            && not game.IsOver 
            && game.State = GameState.Turn(OTurn)) then game.TakeAITurn()
        Assert.IsTrue game.IsOver
        match game.State with
        | FinalState s -> Assert.AreEqual(endCondition, s.EndCondition)
        | _ -> Assert.Fail()

    let executeStateTestSequence sequence =
        sequence |> Seq.iter(fun ( moves, endResult) -> playGame false None moves endResult )

    let executeAiTestSequence sequence =
        sequence |> Seq.iter(fun ( moves, endResult, startingTurn) -> playGame true startingTurn moves endResult )
