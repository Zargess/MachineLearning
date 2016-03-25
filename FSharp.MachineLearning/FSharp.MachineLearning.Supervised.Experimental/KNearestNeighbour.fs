namespace FSharp.MachineLearning.Supervised.Experimental

open FSharp.MachineLearning

module KNearestNeighbour =
    let euclidean (point : float list) (dpoint : float list) =
        List.zip point dpoint
        |> List.fold (fun sum (x, y) -> sum + pown (x - y) 2) 0.0
        |> sqrt

    let rec countOccurrencesOfLabel label data counter =
        match data with
        | [] -> (label, counter)
        | (l, d)::xs when l = label -> countOccurrencesOfLabel label xs (counter + 1)
        | x::xs -> countOccurrencesOfLabel label xs counter
        
    let vote data =
        Seq.distinctBy (fun (l, d) -> l) data
        |> Seq.map (fun (l, d) -> countOccurrencesOfLabel l data 0)
        |> Seq.maxBy (fun (l, c) -> c)

    let rec validIndex index list = 
        let length = List.length list
        length > 0 && index >= 0 && index < length

    let column (dataset : float list list) i = List.map (fun (row : float list) -> row.[i]) dataset

    let columns (dataset : float list list) =
        let cols = dataset.[0] |> List.length
        [ for i in 0 .. (cols - 1) -> column dataset i ]

    let minMax dataset =
        dataset
        |> columns
        |> List.map (fun list -> List.min list, List.max list)

    let minMaxNormalizer dataset =
        let bounds = minMax dataset
        fun (point : float list) ->
            List.mapi (fun i (min, max) -> (point.[i] - min) / (max - min)) bounds

    let normalize dataset normalizer =
        dataset
        |> List.map normalizer

    let classify target data labels k =
        List.map (euclidean target) data
        |> List.zip labels
        |> List.sortBy (fun (l, d) -> d)
        |> List.getNFirstElements k
        |> vote 