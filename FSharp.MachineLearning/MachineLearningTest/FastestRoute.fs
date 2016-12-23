namespace MachineLearningTestCases

open Zargess.MachineLearning.ReinforcementLearning

module FastestRoute =
    let initialWorld = [""; ""; ""; ""; ""; "";]

    let initialState = {
        world = initialWorld;
        reward = 0.0;
        agent = { id = "Humus" }
    }

    let neutrualAction = { value = -1; }

    let getNextAgent (agent : Agent) = agent

    let performAction (state : State<string list>) (action : Action) = {
        world = List.replaceAt initialWorld state.agent.id action.value;
        reward = state.reward;
        agent = state.agent
    }

    let isWinningState (state : State<string list>) =
        let pos = Utility.findAgentPosition state
        match pos with
        | 5 -> true
        | _ -> false

    let rewardFunction (state : State<string list>) = 
        match isWinningState state with
        | true -> 100.0
        | false -> 0.0

    let getAvailableActions (actionMap : Map<int, int list>) (state : State<string list>) =
        let position = Utility.findAgentPosition state
        actionMap.[position]
        |> List.map (fun x -> { value = x; })

    (* TODO : Consider rewriting this so it does not return a 3 tuple *)
    let lookupFunction (map : Map<(State<string list> * Action), float>) (state : State<string list>) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> map.[(state, action)]
        | false -> 0.0

    let calcEpsilon x = -0.0001 * x + 1.0

    let getRandomStartState (random : System.Random) =
        let action = {
            value = random.Next(0, 6)
        }
        performAction initialState action
    
    let isEndState (state : State<string list>) = isWinningState state

    let run () =
        let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [0; 3; 5;]); (5, [1; 4;]); ]
        let getActions = getAvailableActions actionMap
        let random = new System.Random()
        let alpha = 0.5
        let gamma = 1.0
        let startState = performAction initialState { value = 3; }

        let getStartState () = getRandomStartState random

        let gameconfig : GameConfiguration<string list> = {
            isEndState = isEndState;
            getActions = getActions;
            performAction = performAction;
            rewardFunction = rewardFunction;
            lookupFunction = lookupFunction;
            calcEpsilon = calcEpsilon;
            getNextAgent = getNextAgent;
            random = random;
            neutrualAction = neutrualAction;
            getStartState = getStartState;
            learningRate = alpha;
            discountFactor = gamma
        }

        let Q = QLearning.learn gameconfig 50000

        let lookup = lookupFunction Q

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
                    | hd::tl -> QLearning.getActionGreedy lookup currentState actions
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
                        printf "%O" (Utility.findAgentPosition state)
                        printf "%O" "->"
                    printf "%O" "end\n"
                    inputFromUser false
                | _ -> inputFromUser false

        inputFromUser false
        Q
