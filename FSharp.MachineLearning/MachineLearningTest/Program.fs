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

    let neutrualAction = { value = -1; }

    let getNextAgent (agent : Agent) = agent

    let performAction (state : State) (action : Action) = {
        world = replaceAt initialWorld state.agent.id action.value;
        reward = state.reward;
        agent = state.agent
    }

    let rewardFunction won (state : State) = 
        match won with
        | true -> 100.0
        | false -> 0.0

    let getAvailableActions (actionMap : Map<int, int list>) (state : State) =
        let position = findAgentPosition state
        actionMap.[position]
        |> List.map (fun x -> { value = x; })

    (* TODO : Consider rewriting this so it does not return a 3 tuple *)
    let lookup (map : Map<(State * Action), float>) (state : State) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> state, action, map.[(state, action)]
        | false -> state, action, 0.0

    let isWinningState (state : State) =
        let pos = findAgentPosition state
        match pos with
        | 5 -> true
        | _ -> false

    let calcEpsilon x = -0.0001 * x + 1.0

    let getRandomStartState (random : System.Random) =
        let action = {
            value = random.Next(0, 6)
        }
        performAction initialState action

    (*
        TODO : Make a print board function
    *)

    let run =
        let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [0; 3; 5;]); (5, [1; 4;]); ]
        let getActions = getAvailableActions actionMap
        let random = new System.Random()
        let alpha = 0.5
        let gamma = 1.0
        let startState = performAction initialState { value = 3; }

        let Q = learn isWinningState getActions performAction rewardFunction getNextAgent 100 alpha gamma 0.0 neutrualAction lookup Map.empty getRandomStartState random calcEpsilon
        for x in Q do
            printfn "%A" "------------------------------------------------"
            printfn "%A" "State Action Reward tuple:"
            printfn "%A" x.Key
            printfn "%A" x.Value
            printfn "%A" "------------------------------------------------"

            (*
                TODO : Make an interactive step now so that I can give a start state and it will find the fastest route out
            *)


open System
[<EntryPoint>]
let main argv = 
    FastestRouteCase.run
    Console.ReadLine() |> ignore
    0 // return an integer exit code
