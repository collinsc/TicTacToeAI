namespace TicTacToe.Game.Tests


module UnitTests =
    open Microsoft.VisualStudio.TestTools.UnitTesting
    open TicTacToe.Game.GameTypes
    open TestFunctions
    
    [<TestClass>]
    type TestGameStates () =

        [<TestMethod>]
        member this.TestCol1() = 
            executeStateTest(seq { (0, 0); (1, 1); (1, 0); (2, 2); (2, 0) }, EndCondition.Column1)

        [<TestMethod>]
        member this.TestCol2() = 
            executeStateTest(seq { (0, 1); (1, 2); (1, 1); (2, 2); (2, 1) }, EndCondition.Column2)

        [<TestMethod>]
        member this.TestCol3() = 
            executeStateTest(seq { (0, 2); (1, 1); (1, 2); (2, 1); (2, 2) }, EndCondition.Column3)
        
        [<TestMethod>]
        member this.TestRow1() = 
            executeStateTest(seq { (0, 0); (1, 1); (0, 1); (2, 2); (0, 2) }, EndCondition.Row1)

        [<TestMethod>]
        member this.TestRow2() = 
            executeStateTest(seq { (1, 0); (0, 1); (1, 1); (2, 2); (1, 2) }, EndCondition.Row2)

        [<TestMethod>]
        member this.TestRow3() = 
            executeStateTest(seq { (2, 0); (0, 1); (2, 1); (1, 2); (2, 2) }, EndCondition.Row3)
            

        [<TestMethod>]
        member this.TestMajorDiagonal() = 
            executeStateTest(seq { (0, 0); (0, 1); (1, 1); (0, 2); (2, 2) }, EndCondition.DiagonalMajor)

        [<TestMethod>]
        member this.TestMinorDiagonal() = 
            executeStateTest(seq { (0, 2); (0, 1); (1, 1); (1, 2); (2, 0) }, EndCondition.DiagonalMinor)
    
        [<TestMethod>]
        member this.TestDraw() = 
            executeStateTest(
                seq { (0, 0); (0, 1); (1, 1); (2, 2); (1, 0); (2, 0); (2, 1); (1, 2); (0, 2) }, 
                EndCondition.Draw)

    
    [<TestClass>]
    type TestAI () =

        [<TestMethod>]
        member this.TestAIWinRow1Going2nd() =
            executeAiTest(seq { (1, 1); (1, 0); (2, 1); (2, 0) }, EndCondition.Row1, Some(XTurn))

        [<TestMethod>]
        member this.TestAIWinCol1Going2nd() =
            executeAiTest(seq { (1, 1); (1, 2); (0, 2)}, EndCondition.Column1, Some(XTurn))

        [<TestMethod>]
        member this.TestAIWinDiagonalMajorGoing1st() =
            executeAiTest(seq { (0, 1); (2, 0); (1, 2) }, EndCondition.DiagonalMajor, Some(OTurn))

        [<TestMethod>]
        member this.TestAIDrawBestOpeningGoing2nd() =
            executeAiTest(seq {(0, 0); (2, 2); (2, 1); (0, 2); (1, 0)}, EndCondition.Draw, Some(XTurn))

        [<TestMethod>]
        member this.TestAIDrawCenterOpeningGoing2nd() =
            executeAiTest(seq {(1, 1); (1, 0); (0, 2); (2, 1); (2, 2)}, EndCondition.Draw, Some(XTurn))

        [<TestMethod>]
        member this.TestAIDrawGoing1st() =
            executeAiTest(seq {(1, 1); (0, 2); (1, 0); (2, 1)        }, EndCondition.Draw, Some(OTurn))
