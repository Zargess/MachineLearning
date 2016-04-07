namespace MachineLearningTestCases
open FSharp.MachineLearning.Supervised.Experimental
open FSharp.MachineLearning
open System.IO

module Digit =

    let training = "C:\Users\Marcus\OneDrive\Dokumenter\Digits\\trainingDigits"
    let testdata = "C:\Users\Marcus\OneDrive\Dokumenter\Digits\\testDigits"

    let stringToFloatList (s : string) =
        s.ToCharArray(0, s.Length)
        |> List.ofArray
        |> List.map float

    let readFileToList (filepath : string) = 
        File.ReadAllLines(filepath)
        |> List.ofArray
        |> List.fold (fun state x -> x + state) ""
        |> stringToFloatList
        
    let testClassifier k data labels (point, label) =
        match KNearestNeighbour.classify point data labels k with
        | (l, count) when l = label -> 0
        | (l, count) when l <> label -> 1
        | _ -> failwith "what the fuck?"

    let getLabel (filepath : string) =
        let filename = filepath.Split [|'\\'|]
                       |> List.ofArray
                       |> List.getLastElement
        filename.[0].ToString()

    let loadData (path : string) =
        Directory.GetFiles(path)
        |> List.ofArray
        |> List.map (fun x -> (readFileToList x, getLabel x))
        |> List.fold (fun (points, labels) (point, label) -> point::points, label::labels) ([], [])

    let run () =
        let data, labels = loadData training
        let testdata, testlabels = loadData testdata

        List.zip testdata testlabels
        |> List.map (testClassifier 12 data labels)
        |> List.sum