namespace FSharp.MachineLearning.Reinforcement.Experimental

module Engine =

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

        let newQsa = qsa + alpha * ((currentState.reward + gamma) * (qsap - qsa))

        (previousState, previousAction, newQsa)

    (*
        Finds the action which gives the best expected reward
    *)
    let rec getActionGreedy (lookup : State -> Action -> State * Action * float) currentState actions bestActionSoFar =
        let currentBest = lookup currentState bestActionSoFar
        match actions with
        | [] ->
            let _, result, _ = currentBest
            result
        | hd::tl ->
            let currentActionValue = lookup currentState hd
            match currentActionValue with
            | x when x > currentBest -> getActionGreedy lookup currentState tl hd
            | _ -> getActionGreedy lookup currentState tl bestActionSoFar

    (*
        Finds a random action
    *)
    let getRandomAction (random : System.Random) (actions : Action list) =
        actions.[random.Next(0, actions.Length)]
    
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
            | _ -> Some(getRandomAction random actions)

    (*
        TODO : Consider making the general learning run a general function here
    *)