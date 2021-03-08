namespace TicTacToe.Game

module ABPruning =

    let private prettyNode score value = sprintf "{%d : %A}" score value

    type ABSearchResult<'T> =
        { Score: int
          Value: 'T }

        member this.PrettyString() = prettyNode this.Score this.Value


        static member Default maximizing =
            { Score = if maximizing then System.Int32.MinValue else System.Int32.MaxValue
              Value = Unchecked.defaultof<'T> }



        static member CreateLeaf score value = 
            { Score = score; Value = value}


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

        static member NextSearch currntParams = 
            let maximizing = not(currntParams.Maximizing)
            { currntParams with 
                Depth = currntParams.Depth + 1
                Maximizing = maximizing
                Result = ABSearchResult<'T>.Default maximizing }

    type LazyABSearchTree<'T>=
        abstract member Children : option<seq<LazyABSearchTree<'T>>>
        abstract member GetScore : ABSearchData<'T> -> int
        abstract member Value: 'T

    let private getUpdatedData oldData newValue move = 
        let replaceResult =
            match oldData.Maximizing with
            | true when newValue.Score >= oldData.Result.Score -> true
            | false when newValue.Score <= oldData.Result.Score -> true
            | _ -> false

        let replaceAlpha = oldData.Maximizing && newValue.Score > oldData.Alpha
        let replaceBeta = not(oldData.Maximizing) && newValue.Score < oldData.Beta

        { oldData with 
            Result = if replaceResult then {newValue with Value = move} else oldData.Result
            Alpha = if replaceAlpha then newValue.Score else oldData.Alpha
            Beta = if replaceBeta then newValue.Score else oldData.Beta }


    //
    let rec doABPruningSearch (root:LazyABSearchTree<_>)(parameters:ABSearchData<_>) =
        if parameters.IsMaxDepth || root.Children.IsNone then
            // base case, game over or no more lookahead
            { Score = root.GetScore parameters; Value = root.Value }
        else
            // perform MinMaxABPruning algorithm for this tree
            
            // compute score for subtree and return updated search parameters
            let computeChild searchParams child =  
                let newParams = ABSearchData.NextSearch searchParams
                let subtreeScore = doABPruningSearch child newParams
                getUpdatedData searchParams subtreeScore child.Value

            // if this subtree can't produce a better/worse outcome don't explore it further
            let doStop (value:ABSearchData<_>) = 
                value.IsPruneable

            // setup lazy computation for all children
            let resultSequence =
                Seq.scan computeChild parameters root.Children.Value // lazy computation for the score at each child
                |> Seq.cache                            // cache the results (we might need to get last compuatation)

            // perform computation
            let earlyResult = 
                resultSequence                          // execute the computations
                |> Seq.tryFind(doStop)                  // stop early if AB break condition
            

            if earlyResult.IsSome then  
                // we found best value skip siblings
                earlyResult.Value.Result
            else                        
                // otherwise the last result has the best value out of all children
                (resultSequence |> Seq.last).Result

