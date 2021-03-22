namespace TicTacToe.Game.Tests

module TestFunctions = 
    open Microsoft.VisualStudio.TestTools.UnitTesting
    open TicTacToe.Game
    open GameTypes
    open MutableState
    open GameLogic

    let log = true

    let takeHumanTurn (game:Game) row col = 
        game.TakeTurn row col
        if log then printfn "%s" (game.ToString())

    let takeAITurn (game:Game)= 
        game.TakeDeterministicAITurn()
        if log then printfn "%s" (game.ToString())

    // plays the game, executes a sequence of moves, and asserts end condtion
    let playGame doAi startingTurn moves endCondition =
        let game = Game(startingTurn)

            
        let turnOrder = 
            game.State 
            |> Seq.unfold 
                (fun state ->
                    match state with 
                    | _ when doAi -> Some(State.Turn(startingTurn.Value), State.Turn(startingTurn.Value))
                    | State.Turn(Player.X) -> Some(State.Turn(Player.O), State.Turn(Player.O))
                    | State.Turn(Player.O) -> Some(State.Turn(Player.X), State.Turn(Player.X))
                    | _ -> None)

        for (row, col), turn in Seq.zip(moves)(turnOrder) do
            Assert.IsFalse game.IsOver
            match turn with 
            | State.Turn(Player.O) when doAi -> 
                takeAITurn game
                if not(game.IsOver) then 
                    takeHumanTurn game row col
            | State.Turn(Player.X) when doAi -> 
                takeHumanTurn game row col
                if not (game.IsOver) then 
                    takeAITurn game
            | _ -> takeHumanTurn game row col
            if not(game.IsOver) then Assert.AreEqual(turn, game.State) 

        let aiLastTurn = 
            doAi 
            && startingTurn.IsSome 
            && startingTurn.Value = Player.O 
            && game.State = State.Turn(Player.O)
        // fencepost
        if (aiLastTurn && not game.IsOver ) then 
            takeAITurn game
        Assert.IsTrue game.IsOver
        match game.State with
        | FinalState s -> Assert.AreEqual(endCondition, s.EndCondition)
        | _ -> Assert.Fail()

    let executeStateTest(moves, endResult) = playGame false None moves endResult 


    let executeAiTest( moves, endResult, startingTurn) = playGame true startingTurn moves endResult
