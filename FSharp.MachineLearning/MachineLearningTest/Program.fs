
open MachineLearningTestCases



do
    printfn "%s" "Starting"
    
    let Q = TicTacToe.run ()
    //let Q = FastestRoute.run ()
    let pairs = Map.filter (fun key value -> value > 0.0) Q
    for p in pairs do
        printfn "%O" "--------------------------------------------------------------"
        printfn "%A" p.Key
        printfn "%A" p.Value

//    printfn "%A" (DatingSite.run ())
    
    //printfn "%A" (Digit.run ())

    System.Console.ReadLine() |> ignore