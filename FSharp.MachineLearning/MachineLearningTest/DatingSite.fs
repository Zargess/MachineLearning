namespace MachineLearningTestCases

open System.IO
open FSharp.MachineLearning.Supervised.Experimental
open FSharp.MachineLearning

module DatingSite =

    let readLines (filepath : string) = File.ReadAllLines(filepath)

    let dataPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet.txt"
    let testPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet2.txt"
    
    let getPoint list =
        list
        |> List.getNFirstElements 3
        |> List.map float

    let loadData (path : string) =
        path
        |> readLines
        |> List.ofArray
        |> List.map (fun x -> x.Split [|'\t'|] |> List.ofArray)
        |> List.map (fun list -> (getPoint list, List.getLastElement list))
        |> List.fold (fun (points, labels) (point, label) -> point::points, label::labels) ([], [])

    let testClassifier k data labels (point, label) =
        match KNearestNeighbour.classify point data labels k with
        | (l, count) when l = label -> 0
        | (l, count) when l <> label -> 1
        | _ -> failwith "what the fuck?"

    let mapLabel l =
        match l with
        | "1" -> "didntLike"
        | "2" -> "smallDoses"
        | "3" -> "largeDoses"
        | _   -> "foo"

    let run () =
        let data, labels = loadData dataPath

        let testdata, testLabels = loadData testPath

        let normalizer = KNearestNeighbour.minMaxNormalizer
        let normData = KNearestNeighbour.normalize data (normalizer data)
        let normTestData = KNearestNeighbour.normalize testdata (normalizer testdata)

        List.map mapLabel testLabels
        |> List.zip normTestData
        |> List.map (testClassifier 12 normData labels)
        |> List.sum