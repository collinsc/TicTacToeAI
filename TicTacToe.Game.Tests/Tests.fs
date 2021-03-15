namespace TicTacToe.Game.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open TicTacToe.Game.GameTypes
open TestFunctions

[<TestClass>]
type TestGameStates () =

    [<TestMethod>]
    member this.TestColWins() = 
        seq { 
            (seq { (0,0); (1,1); (1,0); (2,2); (2,0) }, EndCondition.Column1)
            (seq { (0,1); (1,2); (1,1); (2,2); (2,1) }, EndCondition.Column2)
            (seq { (0,2); (1,1); (1,2); (2,1); (2,2) }, EndCondition.Column3)
            } |> executeStateTestSequence

    [<TestMethod>]
    member this.TestRowWins() = 
        seq { 
            (seq { (0,0); (1,1); (0,1); (2,2); (0,2) }, EndCondition.Row1) 
            (seq { (1,0); (0,1); (1,1); (2,2); (1,2) }, EndCondition.Row2)
            (seq { (2,0); (0,1); (2,1); (1,2); (2,2) }, EndCondition.Row3)
            } |> executeStateTestSequence

    [<TestMethod>]     
    member this.TestDiagWins() = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (0,2); (2,2) }, EndCondition.DiagonalMajor)
            (seq { (0,2); (0,1); (1,1); (1,2); (2,0) }, EndCondition.DiagonalMinor)
            } |> executeStateTestSequence

    [<TestMethod>]    
    member this.TestDraw() = 
        seq { 
            (seq { (0,0); (0,1); (1,1); (2,2); (1,0); (2,0); (2,1); (1,2); (0,2) }, EndCondition.Draw)
            } |> executeStateTestSequence

[<TestClass>]
type TestAI () =

    [<TestMethod>]
    member this.TestAiWins() = 
        seq { 
            (seq {(1,1); (1,0); (2,1); (2,0)       }, EndCondition.Row1, Some(XTurn))
            (seq {(1,1); (1,2); (0,2)              }, EndCondition.Column1, Some(XTurn))
            (seq {(0,1); (2,0); (1,2)              }, EndCondition.DiagonalMajor, Some(OTurn))
            } |> executeAiTestSequence 

    [<TestMethod>]
    member this.TestAiDraw() = 
        seq { 
            (seq {(0,0); (2,2); (2,1); (0,2); (1,0)}, EndCondition.Draw, Some(XTurn))
            (seq {(1,1); (1,0); (0,2); (2,1); (2,2)}, EndCondition.Draw, Some(XTurn))
            (seq {(1,1); (0,2); (1,0); (2,1)       }, EndCondition.Draw, Some(OTurn))
            } |> executeAiTestSequence 

