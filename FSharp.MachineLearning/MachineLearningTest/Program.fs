// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open FSharp.MachineLearning.Reinforcement.Experimental.Engine

module FastestRouteCase =
    let initialWorld = [""; ""; ""; ""; ""; "";]

    let initialState = {
        world = initialWorld;
        reward = 0.0;
        agent = { id = "Humus" }
    }

    let getNextAgent (agent : Agent) = agent

    let performAction (state : State) (action : Action) = 
        replaceAt initialWorld state.agent.id action.value

    let reward won (state : State) = 
        match findAgentPosition state with
        | 5 -> 100.0
        | _ -> 0.0

    (*
        TODO : Consider moving this to the Engine
    *)
    let getAvailableActions (actionMap : Map<int, int list>) (state : State) =
        let position = findAgentPosition state
        actionMap.[position]

    let lookup (map : Map<(State * Action), float>) (state : State) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> map.[(state, action)]
        | false -> 0.0

    let isWinningState (state : State) =
        let pos = findAgentPosition state
        match pos with
        | 5 -> true
        | _ -> false

    (*
        TODO : Make a print board function
    *)

    (*
        TODO : This is is the section where the experiment should run. Make a random number generator
    *)
    let run =
        let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [3; 5;]); (5, [1; 4;]); ]
        let getActions = getAvailableActions actionMap
        let random = new System.Random()

        (*
            TODO : Either do the rest in a loop or in a recursive function
        *)
        let Q : Map<(State * Action), float> = Map.empty
        printfn "%A" Q


open System
[<EntryPoint>]
let main argv = 
    FastestRouteCase.run
    Console.ReadLine() |> ignore
    0 // return an integer exit code
