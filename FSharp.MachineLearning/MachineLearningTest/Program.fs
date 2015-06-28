﻿open FSharp.MachineLearning.Reinforcement.Experimental.Engine

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

    let run () =
        let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [0; 3; 5;]); (5, [1; 4;]); ]
        let getActions = getAvailableActions actionMap
        let random = new System.Random()
        let alpha = 0.5
        let gamma = 1.0
        let startState = performAction initialState { value = 3; }

        let Q = learn isWinningState isWinningState getActions performAction rewardFunction getNextAgent 100 alpha gamma 0.0 neutrualAction lookup Map.empty getRandomStartState random calcEpsilon

        let validInput (input : string) =
            try
                let inputAsInt = int input
                inputAsInt < 6 && inputAsInt >= 0
            with _ -> false

        let rec findWayOut Q currentState routeSoFar =
            let newRoute = currentState::routeSoFar
            match (isWinningState currentState) with
            | true -> newRoute
            | false ->
                let actions = getActions currentState
                let action =    
                    match actions with 
                    | [] -> failwith "No actions available"
                    | hd::tl -> findActionWithBestReward Q currentState tl hd
                let nextState = performAction currentState action
                findWayOut Q nextState newRoute

        let rec inputFromUser stop =
            match stop with
            | true -> ()
            | false ->
                printfn "%O" "--------------------------------------------------------------"
                printfn "%O" "Input a start state number to let the program find its way out\nwrite quit when you want to stop"
                let input = System.Console.ReadLine()
                match input with
                | "quit" -> inputFromUser true
                | x when validInput x ->
                    printfn "%O" "\nRoute: "
                    let number = int input
                    let startState =
                        let action = { value = number }
                        performAction initialState action
                    let route = List.rev (findWayOut Q startState [])
                    for state in route do
                        printf "%O" (findAgentPosition state)
                        printf "%O" "->"
                    printf "%O" "end\n"
                    inputFromUser false
                | _ -> inputFromUser false

        inputFromUser false


module TicTacToe =
    let winningTuples = [ (0,1,2); (3,4,5); (6,7,8); (0,3,6); (1,4,7); (2,5,8); (0,4,8); (2,4,6); ]
    let initialState = {
        world = [ ""; ""; ""; ""; ""; ""; ""; ""; ""; ];
        reward = 0.0;
        agent = { id="x" }
    }

    let neutrualAction = {
        value = -1
    }

    let getNextAgent agent = agent

    let isWinningState state =
        let id = state.agent.id
        let rec checkWorld id (world : string list) winningTuples =
            match winningTuples with
            | [] -> false
            | hd::tl ->
                let x,y,z = hd
                let won = world.[x] = id && world.[y] = id && world.[z] = id
                match won with
                | true -> won
                | false -> checkWorld id world tl
        checkWorld id state.world winningTuples

    let isEndState state =
        let freeSpaces =
            List.filter (fun x -> x = "") state.world
            |> List.length

        let isFull = freeSpaces = 0
        let won = isWinningState state

        isFull || won

    let rewardFunction won state =
        match won with
        | true -> 100.0
        | false -> 0.0

    let random = new System.Random()

    let getAvailableActions state = 
        findAllIndicies (fun x -> x = "") state.world [] 0
        |> List.map (fun x -> { value = x })

    (* TODO : Fix the usage of rewardFunction in this function *)
    (* TODO : Make another version that does not perform a random action *)
    let performActionWithRandom (state : State) (action : Action) =
        match action.value with
        | x when x < 0 -> state
        | _ ->
            let tempState = {
                world = replaceAt state.world state.agent.id action.value;
                reward = rewardFunction (isWinningState state) state;
                agent = state.agent
            }
            let actions = getAvailableActions tempState
            let randAction = getRandomAction random actions
            match randAction with
            | None -> tempState
            | Some(x) -> { world = replaceAt tempState.world "o" x.value; reward = rewardFunction (isWinningState state) state; agent = state.agent }

    let performAction (state : State) (action : Action) =
        match action.value with
        | x when x < 0 -> state
        | _ -> {
                world = replaceAt state.world state.agent.id action.value;
                reward = rewardFunction (isWinningState state) state;
                agent = state.agent
            }

    let lookup (map : Map<(State * Action), float>) (state : State) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> state, action, map.[(state, action)]
        | false -> state, action, 0.0

    let calcEpsilon x = -0.0001 * x + 1.0

    let getRandomStartState random = initialState

    let printBoard state =
        printfn "%O" "------------------"
        printfn "%O" (state.world.[0] + "\t" + state.world.[1] + "\t" + state.world.[2])
        printfn "%O" "------------------"
        printfn "%O" (state.world.[3] + "\t" + state.world.[4] + "\t" + state.world.[5])
        printfn "%O" "------------------"
        printfn "%O" (state.world.[6] + "\t" + state.world.[7] + "\t" + state.world.[8])
        printfn "%O" "------------------"

    let run () =
        let alpha = 0.5
        let gamma = 1.0

        let Q = learn isWinningState isEndState getAvailableActions performAction rewardFunction getNextAgent 50000 alpha gamma 0.0 neutrualAction lookup Map.empty getRandomStartState random calcEpsilon

        let validInput (input : string) =
            try
                let s = int input
                s < 9 && s >= 0
            with _ -> false

        let rec playGame Q state ended =
            printBoard state
            match ended with
            | true -> state
            | false ->
                let input = System.Console.ReadLine()
                match validInput input with
                | false -> playGame Q state ended
                | true ->
                    let action = { value = (int input); }
                    let newState = performAction state action
                    computerMove Q newState (isEndState newState)

        and computerMove Q state ended =
            match ended with
            | true -> state
            | false ->
                let action =
                        match getAvailableActions state with
                        | [] -> failwith "No actions available"
                        | hd::tl ->
                            findActionWithBestReward Q state tl hd
                let newState = performAction initialState action
                playGame Q newState (isEndState state)

        let rec inputFromUser stop =
            match stop with
            | true -> ()
            | false ->
                printfn "%O" "--------------------------------------------------------------"
                printfn "%O" "Write start to start a new game or quit to exit game."
                let newGameCommand = System.Console.ReadLine()

                match newGameCommand with
                | "quit" -> inputFromUser true
                | "start" ->
                    printfn "%O" "--------------------------------------------------------------"
                    printfn "%O" "A new game has started. The computer takes its turn.\nOn your turn write the number of a valid action to play the game."
                    let action =
                        match getAvailableActions initialState with
                        | [] -> failwith "No actions available"
                        | hd::tl ->
                            findActionWithBestReward Q initialState tl hd
                    let newState = performAction initialState action
                    playGame Q newState false |> ignore
                    inputFromUser false
                | _ -> inputFromUser false
             
        inputFromUser false       
        Q

do
    printfn "%s" "Starting"
    
    let Q = TicTacToe.run ()
    let pairs = Map.filter (fun key value -> value > 0.0) Q
    for p in pairs do
        printfn "%O" "--------------------------------------------------------------"
        printfn "%A" p.Key
        printfn "%A" p.Value
    System.Console.ReadLine() |> ignore