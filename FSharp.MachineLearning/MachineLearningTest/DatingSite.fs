namespace MachineLearningTestCases

module DatingSite =
    open System.IO
    open FSharp.MachineLearning.Supervised.Experimental

    let readLines (filepath : string) = File.ReadAllLines(filepath)

    let dataPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet.txt";
    let testPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet2.txt"

    let (|DataPoint|_|) input  =
        match input with
        | [x;y;z;l] -> Some ([x;y;z], l)
        | _ -> None

    let getData (list : string list) =
        match list with
        | DataPoint (point, label) -> (label, List.map float point)
        | _ -> failwith "List is not of "

    let testClassifier k data (label, point) =
        match KNearestNeighbour.classify point data k with
        | (l, count) when l = label -> 0
        | (l, count) when l <> label -> 1
        | _ -> failwith "what the fuck?"

    let mapLabel (l, point) =
        match l with
        | "1" -> ("didntLike", point)
        | "2" -> ("smallDoses", point)
        | "3" -> ("largeDoses", point)
        | _   -> ("foo", point)

    let run () =
        let data =
            readLines dataPath
            |> Seq.toList
            |> List.map (fun x -> x.Split [|'\t'|])
            |> List.map List.ofArray
            |> List.map getData

        let test =
            readLines testPath
            |> Seq.toList
            |> List.map (fun x -> x.Split [|'\t'|])
            |> List.map List.ofArray
            |> List.map getData
            |> List.map mapLabel

        List.map (testClassifier 12 data) test
        |> List.sum