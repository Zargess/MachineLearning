namespace MachineLearningTestCases

open Zargess.MachineLearning.ReinforcementLearning

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

    let player = { id = "o" }
    let computer = { id = "x" }

    let getNextAgent agent = agent

    let isWinningState (player : Agent) (state : State) =
        let id = player.id
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
        let won = isWinningState computer state
        let lost = isWinningState player state

        isFull || won

    let rewardFunction state = 
        match (isWinningState computer state, isWinningState player state) with
        | (true, _) -> 100.0
        | (_, true) -> -100.0
        | _ -> 0.0

    let random = new System.Random()

    let getAvailableActions state = 
        List.findAllIndicies (fun x -> x = "") state.world [] 0
        |> List.map (fun x -> { value = x })

    (* TODO : Fix the usage of rewardFunction in this function *)
    (* TODO : Make another version that does not perform a random action *)
    let performActionWithRandom (state : State) (action : Action) =
        match action.value with
        | x when x < 0 -> state
        | _ ->
            let tempState = {
                world = List.replaceAt state.world state.agent.id action.value;
                reward = rewardFunction state;
                agent = state.agent
            }
            let actions = getAvailableActions tempState
            let randAction = QLearning.getRandomAction random actions
            match randAction with
            | None -> tempState
            | Some(x) -> { world = List.replaceAt tempState.world "o" x.value; reward = rewardFunction state; agent = state.agent }

    let performAction (state : State) (action : Action) (agent : Agent) =
        match action.value with
        | x when x < 0 -> state
        | _ -> {
                world = List.replaceAt state.world state.agent.id action.value;
                reward = rewardFunction state;
                agent = agent
            }

    let lookup (map : Map<(State * Action), float>) (state : State) (action : Action) =
        match map.ContainsKey((state, action)) with
        | true -> map.[(state, action)]
        | false -> 0.0

    let calcEpsilon x = -0.0001 * x + 1.0

    let getRandomStartState random = initialState

    let getStartState () = initialState

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

        let gameconfig : GameConfiguration = {
            isEndState = isEndState;
            getActions = getAvailableActions;
            performAction = performActionWithRandom;
            rewardFunction = rewardFunction;
            lookupFunction = lookup;
            calcEpsilon = calcEpsilon;
            getNextAgent = getNextAgent;
            random = random;
            neutrualAction = neutrualAction;
            getStartState = getStartState;
            alpha = alpha;
            gamma = gamma
        }

        let Q = QLearning.learn gameconfig Map.empty 0.0 500000

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
                    let newState = performAction state action computer
                    computerMove Q newState (isEndState newState)

        and computerMove Q state ended =
            match ended with
            | true -> state
            | false ->
                let action =
                        match getAvailableActions state with
                        | [] -> failwith "No actions available"
                        | hd::tl -> QLearning.getActionGreedy Q state tl hd
                let newState = performAction state action player
                playGame Q newState (isEndState newState)

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
                            QLearning.getActionGreedy (lookup Q) initialState tl hd
                    let newState = performAction initialState action player
                    playGame (lookup Q) newState false |> ignore
                    inputFromUser false
                | _ -> inputFromUser false
             
        inputFromUser false       
        Q
