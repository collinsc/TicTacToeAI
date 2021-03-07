namespace TicTacToeGame

module ABPruning =

    type ABSearchResult<'T> =
        { Score: int
          Value: 'T }

        static member Default maximizing =
            { Score = if maximizing then System.Int32.MinValue else System.Int32.MaxValue
              Value = Unchecked.defaultof<'T> }


    type ABSearchData<'T> = 
        { Maximizing: bool
          Depth: int
          MaxDepth: int 
          Alpha: int
          Beta: int  
          Result : ABSearchResult<'T> }

        member this.IsMaxDepth = this.Depth >= this.MaxDepth

        member this.IsPruneable = this.Alpha >= this.Beta

        static member Default maxDepth = 
            { Maximizing = true
              Depth = 0
              MaxDepth = maxDepth
              Alpha = System.Int32.MinValue
              Beta = System.Int32.MaxValue
              Result = ABSearchResult.Default true }

    type LazyABSearchTree<'T>=
        abstract member Children : option<seq<LazyABSearchTree<'T>>>
        abstract member GetScore : ABSearchData<'T> -> int
        abstract member Value: 'T

    let private getUpdatedData oldData newValue = 
        { oldData with 
            Result = 
                match oldData.Maximizing with 
                | true when newValue.Score > oldData.Result.Score -> newValue
                | false when newValue.Score < oldData.Result.Score -> newValue
                | _ -> oldData.Result
            Alpha = 
                match oldData.Maximizing with 
                | true when newValue.Score > oldData.Alpha -> newValue.Score
                | _ -> oldData.Alpha
            Beta =
                match oldData.Maximizing with 
                | false when newValue.Score < oldData.Beta -> newValue.Score
                | _ -> oldData.Beta }


    
    let rec doABPruningSearch (root:LazyABSearchTree<_>)(parameters:ABSearchData<_>) =
        if parameters.IsMaxDepth || root.Children.IsNone then
            { Score = root.GetScore parameters; Value = root.Value }
        else
            let startingParams = 
                { parameters with 
                    Depth = parameters.Depth + 1 
                    Maximizing = not(parameters.Maximizing)
                    Result = ABSearchResult.Default parameters.Maximizing}

            let computeChild searchParams child =  
                let subtreeScore = doABPruningSearch child searchParams
                getUpdatedData searchParams subtreeScore

            let resultSequence =
                root.Children.Value
                |> Seq.scan computeChild startingParams // lazy computation for the score at each child
                |> Seq.cache                            // cache the results (we might need to get last compuatation)


            // if this subtree can't produce a better/worse outcome don't explore it further
            let doStop (value:ABSearchData<_>) = value.IsPruneable

            let earlyResult = 
                resultSequence                          // execute the computations
                |> Seq.tryFind(doStop)                  // stop early if AB break condition

            if earlyResult.IsSome then 
                earlyResult.Value.Result
            else
                // otherwise the last result has the best/worst value out of all children
                (resultSequence |> Seq.last).Result