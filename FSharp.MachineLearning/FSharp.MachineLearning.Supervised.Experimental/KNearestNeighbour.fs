namespace FSharp.MachineLearning.Supervised.Experimental

module KNearestNeighbour =

    let square x = x * x

    let euclidean (point : float list) (l, dpoint) =
        let rec work (point : float list) (dpoint : float list) (sofar : float) =
            match (point, dpoint) with
            | ([], []) -> (l, sqrt sofar)
            | (x::xs, y::ys) -> work xs ys ((square (x - y)) + sofar)
            | _ -> failwith "The given points are not of correct input"
        work point dpoint 0.0

    let rec countOccurrencesOfLabel label data counter =
        match data with
        | [] -> (label, counter)
        | (l, d)::xs when l = label -> countOccurrencesOfLabel label xs (counter + 1)
        | x::xs -> countOccurrencesOfLabel label xs counter
        
    let vote data =
        Seq.distinctBy (fun (l, d) -> l) data
        |> Seq.map (fun (l, d) -> countOccurrencesOfLabel l data 0)
        |> Seq.maxBy (fun (l, c) -> c)

    let getNFirstElements n list =
        let rec work list n res =
            match list with
            | [] when n = 0 -> res
            | x::xs when n > 0 -> work xs (n-1) (x::res)
            | _ when n = 0 -> res
            | _ -> failwith "n is larger than number of elements"
        work list n []
        |> List.rev

    let rec validIndex index list = 
        let length = List.length list
        length > 0 && index >= 0 && index < length

    let autoNorm (data : ('a * float list) list) column =
        let bigEnough = List.map (fun (_, list) -> validIndex column list) data
                        |> List.fold (fun x y -> x && y) true
        match bigEnough with
        | false -> failwith "invalid column"
        | true ->   
            let min = List.map (fun (_, (list : float list)) -> list.[column]) data
                      |> List.min
            min

    let classify target data k =
        List.map (euclidean target) data
        |> List.sortBy (fun (l, d) -> d)
        |> getNFirstElements k
        |> vote 