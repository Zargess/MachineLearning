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

    let performAction (state : State) (action : Action) = {
        world = replaceAt initialWorld state.agent.id action.value;
        reward = state.reward;
        agent = state.agent
    }

    let rewardFunction won (state : State) = 
        match won with
        | true -> 100.0
        | false -> 0.0

    (*
        TODO : Consider moving this to the Engine
    *)
    let getAvailableActions (actionMap : Map<int, int list>) (state : State) =
        let position = findAgentPosition state
        actionMap.[position]
        |> List.map (fun x -> { value = x; })

    let lookup (map : Map<(State * Action), float>) (state : State) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> state, action, map.[(state, action)]
        | false -> state, action, 0.0

    let isWinningState (state : State) =
        let pos = findAgentPosition state
        match pos with
        | 5 -> true
        | _ -> false

    (*
        TODO : Make a print board function
    *)

    (*
        TODO : This is is the section where the experiment should run.
    *)
    let run =
        let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [3; 5;]); (5, [1; 4;]); ]
        let getActions = getAvailableActions actionMap
        let random = new System.Random()
        let alpha = 0.5
        let gamma = 1.0
        let epsilon = 1.0
        let counter = 1
        let startState = performAction initialState { value = 2; }

        (*
            TODO : Perform doLearningStep even if in the winning state to update the second to last state
        *)
        let rec teach alpha gamma epsilon counter (Q : Map<(State * Action), float>) history currentState : Map<(State * Action), float> =
            let isDone = isWinningState currentState
            
            match isDone with
            | true -> Q
            | false ->
                let lookupFunction = lookup Q
                let action = 
                    let option = getActionEGreedy random lookupFunction (getActions currentState) epsilon currentState
                    match option with
                    | None -> failwith "No action was found"
                    | Some(x) -> x

                let reward, newQ = 
                    match history with
                    | [] -> 0.0, Q
                    | hd::tl ->
                        let prevState, prevAction = hd
                        let _, _, re = doLearningStep alpha gamma lookupFunction isDone prevState prevAction currentState action
                        let nq = Q.Add ((prevState, prevAction), re)
                        re, nq

                let newHistory = (currentState, action) :: history
                let newState, _ = doAction performAction getActions isWinningState rewardFunction getNextAgent currentState action

                teach alpha gamma epsilon counter newQ newHistory newState


        (*
            TODO : Either do the rest in a loop or in a recursive function
        *)
        printfn "%A" (teach alpha gamma epsilon counter Map.empty [] startState)


open System
[<EntryPoint>]
let main argv = 
    FastestRouteCase.run
    Console.ReadLine() |> ignore
    0 // return an integer exit code
