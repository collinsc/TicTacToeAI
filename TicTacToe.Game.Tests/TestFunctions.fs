namespace TicTacToe.Game.Tests

module TestFunctions = 
    open Microsoft.VisualStudio.TestTools.UnitTesting
    open TicTacToe.Game.GameTypes
    open TicTacToe.Game

    let playGame doAi startingTurn moves endCondition =
        let game = Game(startingTurn)
            
        let turnOrder = 
            game.State 
            |> Seq.unfold 
                (fun state ->
                    match state with 
                    | _ when doAi -> Some(State.Turn(startingTurn.Value), State.Turn(startingTurn.Value))
                    | State.Turn(Turn.XTurn) -> Some(State.Turn(Turn.OTurn),State.Turn(Turn.OTurn))
                    | State.Turn(Turn.OTurn) -> Some(State.Turn(Turn.XTurn),State.Turn(Turn.XTurn))
                    | _ -> None)

        for (row, col), turn in Seq.zip(moves)(turnOrder) do
            Assert.IsFalse game.IsOver
            match turn with 
            | State.Turn(Turn.OTurn) when doAi -> 
                game.TakeAITurn()
                if not(game.IsOver) then 
                    game.TakeTurn row col
            | State.Turn(Turn.XTurn) when doAi -> 
                game.TakeTurn row col
                if not (game.IsOver) then 
                    game.TakeAITurn()
            | _ -> game.TakeTurn row col
            if not(game.IsOver) then Assert.AreEqual(turn, game.State) 
        // fencepost
        if (doAi 
            && startingTurn.IsSome 
            && startingTurn.Value = Turn.OTurn 
            && not game.IsOver 
            && game.State = State.Turn(Turn.OTurn)) then 
                game.TakeAITurn()
        Assert.IsTrue game.IsOver
        match game.State with
        | FinalState s -> Assert.AreEqual(endCondition, s.EndCondition)
        | _ -> Assert.Fail()

    let executeStateTestSequence sequence =
        sequence |> Seq.iter(fun ( moves, endResult) -> playGame false None moves endResult )

    let executeAiTestSequence sequence =
        sequence |> Seq.iter(fun ( moves, endResult, startingTurn) -> playGame true startingTurn moves endResult )
