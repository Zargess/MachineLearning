namespace FSharp.MachineLearning.Reinforcement.Experimental

module Engine =
    (* TODO : Use generic types in id, value and world *)
    type Agent = {
        id : string
    }
    
    type Action = {
        value : int
    }

    type State = {
        world : string list;
        reward : float;
        agent : Agent
    }

    (*
        This function replaces a given element at the given index in a list with a new value
    *)
    let rec replaceAt list value index =
        match list with
        | [] when index = 0 -> [ value; ]
        | [] when index > 0 -> failwith "Index bigger than list size!"
        | car::cdr when index > 0 -> car :: replaceAt cdr value (index - 1)
        | car::cdr when index = 0 -> value :: cdr
        | _ -> failwith "One or more errors occurred!"

    let rec findAllIndicies func list indicies counter =
        match list with
        | [] -> indicies
        | hd::tl ->
            match hd with
            | x when func hd -> findAllIndicies func tl (counter::indicies) (counter + 1)
            | _ -> findAllIndicies func tl indicies (counter + 1)

    (*
        This function returns the current possition of an agent in the world.
        It needs the current state of the game.
        state               := State

        findAgentPosition   := State -> int
    *)
    let findAgentPosition state =
        let id = state.agent.id
        List.findIndex (fun x -> x = id) state.world

    (*
        This function takes 5 arguments.
        performAction       := State -> Action -> string list
        getPossipleActions  := State -> Action list
        isWinningState      := State -> bool
        rewardFunction      := State -> float
        getNextAgent        := Agent -> Agent
        state               := State
        action              := Action

        Function signature:
        doAction            := ((State -> Action -> string list) -> (State -> Action list) -> (State -> bool) -> (State -> float) -> (Agent -> Agent) -> State -> Action -> (State * Action list)
    *)
    let doAction performAction getPossipleActions (isWinningState : State -> bool) rewardFunction getNextAgent state action = 
        let newState = performAction state action
        let won = isWinningState newState
        let reward = rewardFunction won state
        let nextAgent = getNextAgent state.agent
        let resState = {
            world = newState.world;
            reward = reward;
            agent = nextAgent
        }

        let possipleActions = getPossipleActions state

        (resState, possipleActions)

    (*
        Note: Single agent Q-learning
        This function takes 8 arguments:

        alpha               := float
        gamma               := float
        lookup              := State -> Action -> float
        isDone              := bool
        previousState       := State
        previousAction      := Action
        currentState        := State
        currentAction       := Action

        This is the function that does the learning step. It computes the reward for a given state action pair and returns it for the user to save.
    *)
    let doLearningStep alpha gamma lookup isDone previousState previousAction currentState currentAction =
        let _, _, qsa = lookup previousState previousAction

        let qsap =
            match isDone with
            | true -> 0.0
            | false -> 
                let _, _, reward = lookup currentState currentAction
                reward

        let newQsa = qsa + alpha * (currentState.reward + gamma * qsap - qsa)
        (previousState, previousAction, newQsa)

    (*
        Finds the action which gives the best expected reward
    *)
    let rec getActionGreedy (lookup : State -> Action -> State * Action * float) currentState actions bestActionSoFar =
        let _, currentBestAction, currentBestValue = lookup currentState bestActionSoFar
        match actions with
        | [] -> currentBestAction
        | hd::tl ->
            let currentActionValue = lookup currentState hd
            match currentActionValue with
            | _, _, x when x > currentBestValue -> getActionGreedy lookup currentState tl hd
            | _ -> getActionGreedy lookup currentState tl bestActionSoFar

    (*
        Finds a random action
    *)
    let getRandomAction (random : System.Random) (actions : Action list) = 
        match actions with
        | [] -> None
        | _ -> Some(actions.[random.Next(0, actions.Length)])
    
    (*
        Either finds the action with the best expected payoff or a random action.
    *)
    let getActionEGreedy (random : System.Random) (lookup : State -> Action -> State * Action * float) (actions : Action list) epsilon currentState : Option<Action> =
        match actions with
        | [] -> None
        | car::cdr ->
            let randomNumber = random.NextDouble()
            match randomNumber with
            | x when x > epsilon -> Some(getActionGreedy lookup currentState cdr car)
            | _ -> getRandomAction random actions

    let rec learn isWinningState getActions performAction rewardFunction getNextAgent roundsLeft alpha gamma counter neutrualAction lookup Q getRandomStartState random calcEpsilon =
        let rec teach alpha gamma epsilon neutrualAction lookup (Q : Map<(State * Action), float>) history currentState =
            let isDone = isWinningState currentState
            let lookupFunction = lookup Q
            let action = 
                let foundAction = getActionEGreedy random lookupFunction (getActions currentState) epsilon currentState
                match foundAction with
                | None -> neutrualAction
                | Some(x) -> x
            let reward, newQ = 
                match history with
                | [] -> 0.0, (Q.Add ((currentState, action), 0.0))
                | hd::tl ->
                    let prevState, prevAction = hd
                    let _, _, re = doLearningStep alpha gamma lookupFunction isDone prevState prevAction currentState action
                    let nq = Q.Add ((prevState, prevAction), re)
                    re, nq
            match isDone with
            | true -> newQ
            | false -> 
                let newHistory = (currentState, action) :: history
                let newState, _ = doAction performAction getActions isWinningState rewardFunction getNextAgent currentState action
                teach alpha gamma epsilon neutrualAction lookup newQ newHistory newState

        match roundsLeft with
        | 0 -> Q
        | x when x > 0 ->
            let startState = getRandomStartState random
            let newCounter = counter + 1.0
            let epsilon = calcEpsilon newCounter
            let newQ = teach alpha gamma epsilon neutrualAction lookup Q [] startState
            learn isWinningState getActions performAction rewardFunction getNextAgent (roundsLeft - 1) alpha gamma newCounter neutrualAction lookup newQ getRandomStartState random calcEpsilon
        | _ -> failwith "cannot handle negative rounds left"


    let rec findActionWithBestReward (Q : Map<(State * Action), float>) currentState actions bestSoFar =
        match actions with
        | [] -> bestSoFar
        | hd::tl ->
            let bestRewardSoFar = Q.[(currentState, bestSoFar)]
            let headReward = Q.[(currentState, hd)]
            match headReward with
            | x when x > bestRewardSoFar -> findActionWithBestReward Q currentState tl hd
            | _ -> findActionWithBestReward Q currentState tl bestSoFar