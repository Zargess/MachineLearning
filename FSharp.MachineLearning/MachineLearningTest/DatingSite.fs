namespace MachineLearningTestCases

module DatingSite =
    open System.IO

    let readLines (filepath : string) = File.ReadAllLines(filepath)

    let dataPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet.txt";
    let testPath = "C:\Users\Marcus\Documents\GitHub\machinelearninginaction\Ch02\datingTestSet2.txt"

    let run () =
        let data =
            readLines dataPath
            |> Seq.toList
            |> List.map (fun x -> x.Split [|'t'|])
            |> List.map List.ofArray
            |> List.map (fun x -> List.map float x)
        data