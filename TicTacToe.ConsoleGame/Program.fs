namespace TicTacToe.ConsoleGame


module Main =
    open System
    open ConsoleGame
    
    
    let LoopUntilQuit prompt processFunc =
        let getInput _ = 
            printf "\n %s" prompt
            Console.ReadLine()
        
        Seq.initInfinite(getInput)                  // inifinite sequence of user input
        |> Seq.tryFind(processFunc >> not)      // process until execute function returns false
        |> ignore
    
    [<EntryPoint>]
    let Main argv =
        match argv with 
            | _ ->
                let app = ConsoleGame()
                LoopUntilQuit app.InputPrompt app.ProcessInput
        0 // return an integer exit code
