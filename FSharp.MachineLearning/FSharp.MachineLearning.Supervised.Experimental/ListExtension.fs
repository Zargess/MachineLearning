namespace FSharp.MachineLearning
module List =
    let getNFirstElements n list =
        let rec work list n res =
            match list with
            | [] when n = 0 -> res
            | x::xs when n > 0 -> work xs (n-1) (x::res)
            | _ when n = 0 -> res
            | _ -> failwith "n is larger than number of elements"
        work list n []
        |> List.rev

    let rec getLastElement list =
        match list with
        | [] -> failwith "no elements in list"
        | [x] -> x
        | hd::tl -> getLastElement tl