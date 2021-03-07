namespace TicTacToe.BusinessLogic.ConsoleProgram
module Main =
    open System
    open TicTacToe.BusinessLogic.Tests
    open ConsoleGame
    
    
    
    let LoopUntilQuit prompt processFunc =
        let getInput = 
            printf "\n %s" prompt
            Console.ReadLine()
        Seq.initInfinite(fun _ -> getInput)                         // inifinite sequence of user input
            |> Seq.tryFind(fun input -> not(processFunc input))     // process untl execute function returns false
            |> ignore
    
    [<EntryPoint>]
    let Main argv =
        match argv with 
            | [|"Test"|] -> Tests.runAll
            | _ ->
                let app = ConsoleGame()
                LoopUntilQuit app.InputPrompt app.ProcessInput
        0 // return an integer exit code