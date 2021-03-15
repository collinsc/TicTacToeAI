namespace TicTacToe.Game

open System.Collections.Generic
open System

// Genereral purpose AB pruning ai, and a whole lot of fun with recursion. 
module ABPruningAI =

    let private getInitialMinMaxScore maximizing = if maximizing then System.Int32.MinValue else System.Int32.MaxValue

    type MinMaxTreeRoot<'State> = 
        { State:'State
          Maximizing:bool
          Depth:int }

        member this.Child child = 
              { State= child
                Maximizing = not this.Maximizing
                Depth = this.Depth + 1 }

        static member Default initialState =
            { State= initialState
              Maximizing = true
              Depth = 0 }


    // singly linked list representing game
    type TreeTraversal<'Move> =
        { Move:Option<'Move>
          Child:Option<TreeTraversal<'Move>> }

        member this.PrettyString = 
            match this.Child with
            | Some(child) -> sprintf "Move : { %A }, Child : {%s}" this.Move child.PrettyString
            | None -> sprintf "Move : { %A }" this.Move


        static member Default initialMove =
            { Move = initialMove
              Child = None}
        
        member this.AttachChild (child:TreeTraversal<'Move>)  = 
            { this with
                Child =  
                    Some( child ) }

    // output of the algorithm
    type MinMaxResult<'State,'Move> = 
        { Root: MinMaxTreeRoot<'State>
          Traversal : TreeTraversal<'Move> }
        member this.NextMove =
            match this.Traversal.Child with
            | Some(child) when child.Move.IsSome -> child.Move
            | _ -> None



    type PlayableGame<'State, 'Move>=
        // State and Move should both implement fast unambigious equality comparison

        // generate a sequence of legal states from our current state, none if no moves possible or game is over
        abstract member LegalMoves:state:'State -> Option<seq<'Move>>
    
        // scorer provides criteria for evaluating game states, we do the computation, same state should produce same score
        // scores are evaluated as a sum of the scores of every node in a traversal
        abstract member Score: State:MinMaxTreeRoot<'State> -> int

        // compute next state
        abstract member ComputeState:state:'State -> legalMove:'Move -> 'State


    // search parameters and current result
    type ABSearchData<'State,'Move> = 
        { Result: MinMaxResult<'State,'Move>
          MaxDepth: int 
          BestChildScore: int
          Alpha: int
          Beta: int }

        member this.IsMaxDepth = this.Result.Root.Depth >= this.MaxDepth


        member this.IsPruneable = this.Alpha >= this.Beta


        member this.PrettyString = 
            sprintf "{ Depth : %d/%d, Maximizing : %b, BestChildScore : %d BestResult : %s, Alpha : %d, Beta : %d, IsPrunable : %b}"
                this.Result.Root.Depth
                this.MaxDepth
                this.Result.Root.Maximizing
                this.BestChildScore
                this.Result.Traversal.PrettyString
                this.Alpha
                this.Beta
                this.IsPruneable

        static member Default maxDepth initialState = 
            let maximizing = true
            { Result = 
                { Root = MinMaxTreeRoot<'State>.Default initialState
                  Traversal = (TreeTraversal.Default<'Move> None) }
              MaxDepth = maxDepth
              BestChildScore = getInitialMinMaxScore maximizing
              Alpha = getInitialMinMaxScore maximizing
              Beta = getInitialMinMaxScore (not maximizing) }

        member this.ChildParameters newState move= 
            let maximizing = (not this.Result.Root.Maximizing)
            { this with 
                Result = 
                    { Root = this.Result.Root.Child newState
                      Traversal = (TreeTraversal.Default<'Move> move) }
                BestChildScore = getInitialMinMaxScore maximizing }

        // performs ab pruning update step and returns as new record connected to best traversal
        member this.UpdateFromChild (childData:ABSearchData<'State,'Move>)(newScore:int) = 
            let maximizing = this.Result.Root.Maximizing
            let replaceResult = 
                not(childData.IsPruneable) && 
                match maximizing with
                | true when newScore >=  this.BestChildScore -> true
                | false when newScore  <= this.BestChildScore -> true
                | _ -> false

            let replaceAlpha = maximizing && newScore > this.Alpha
            let replaceBeta = not(maximizing) && newScore < this.Beta

            let result = 
                if replaceResult then 
                    { this.Result with 
                        Traversal = this.Result.Traversal.AttachChild childData.Result.Traversal }
                else
                    this.Result

            let newSearchData = 
                { this with 
                            BestChildScore = if replaceResult then newScore else this.BestChildScore
                            Result = result
                            Alpha = if replaceAlpha then newScore else this.Alpha
                            Beta = if replaceBeta then newScore else this.Beta }
            newSearchData


    module Logging =
        let doLog = false
        let logResult name repr depth = 
            printfn "%s{%s : %s}" 
                    (String.init depth (fun _ -> "    "))
                    name
                    repr
        let logTree name (tree:ABSearchData<'State,'Move>) = logResult name tree.PrettyString tree.Result.Root.Depth


    type Memoizer<'a, 'b when 'a : equality> (compute:'a ->'b) =
        // need faste equality comparison for this to be effective
        let cache = Dictionary<'a,'b>()
        member this.GetValue args =
            let value = ref Unchecked.defaultof<'b>
            if not(cache.TryGetValue(args,value)) then 
                value := compute(args)
                cache.Add(args, !value)
            !value

            

    let ComputeSearch(parameters:ABSearchData<'State,'Move>) (game:PlayableGame<'State,'Move>) =

        // score is sum of our result and all of our child results, which makes it s good candidate for memoization
        let rec scoreResult (result:MinMaxResult<'State,'Move>) = 
            let score = game.Score result.Root
            match result.Traversal.Child  with
            | Some(child) when child.Move.IsSome -> 
                let nextState = game.ComputeState result.Root.State child.Move.Value
                let nextResult = { Traversal = child; Root = result.Root.Child nextState }
                score + scoreResult(nextResult)
            | _ -> score



        // perform MinMaxABPruning algorithm for this tree
        let rec doABPruningSearch (parameters:ABSearchData<'State,'Move>) =

            let children = (game.LegalMoves parameters.Result.Root.State)

            if parameters.IsMaxDepth || children.IsNone then
                // base case, game over or no more lookahead in search
                parameters 
            else
                // we need to evaluate the score of our children
                
                // compute score for our child and return updated search parameters
                let computeChild (searchParams:ABSearchData<'State,'Move>)(move:'Move) =  
                    
                    let newState = game.ComputeState searchParams.Result.Root.State move
                    let newParams = searchParams.ChildParameters newState (Some move)

                    let subtreeData = doABPruningSearch newParams

                    let score = scoreResult subtreeData.Result

                    // ABPruning update step
                    let updated = searchParams.UpdateFromChild subtreeData score
                    if Logging.doLog then 
                        Logging.logTree $"Child, Move : {move}, Score : {score}" subtreeData
                        Logging.logTree "CurrentBest" updated
                    updated

                // if this subtree can't produce a better/worse outcome don't explore it further
                let doStop (value:ABSearchData<_,_>) = 
                    if Logging.doLog && value.IsPruneable then
                        Logging.logTree "ABPrune" value
                    value.IsPruneable

                let legalMoves = children.Value

                // setup lazy computation for all children
                let resultSequence =
                    legalMoves
                    // lazy computation for the score at each child
                    |> Seq.scan computeChild parameters 
                    // cache the results (we need last result for algorihtm if we can't break early)
                    |> Seq.cache

                // perform computation
                let earlyResult = 
                    resultSequence                          // execute the computations
                    |> Seq.tryFind(doStop)                  // stop early if AB break condition
                
                let subtreeResults = 
                    if earlyResult.IsSome then  
                        // we already found best value skip siblings
                        earlyResult.Value
                    else                        
                        // otherwise the last result holds the best value out of all children
                        (resultSequence |> Seq.last)

                if Logging.doLog then
                    Logging.logTree "BestSubtree" subtreeResults

                subtreeResults

        let final = doABPruningSearch parameters
        final.Result

