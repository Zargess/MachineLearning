namespace Zargess.MachineLearning.ReinforcementLearning

module QLearning =

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
        let reward = rewardFunction newState
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
        let qsa = lookup previousState previousAction

        let qsap =
            match isDone with
            | true -> 0.0
            | false -> 
                let reward = lookup currentState currentAction
                reward

        let newQsa = qsa + alpha * (currentState.reward + gamma * qsap - qsa)
        (previousState, previousAction, newQsa)

    (*
        Finds the action which gives the best expected reward
    *)
    let rec getActionGreedy (lookup : State -> Action -> float) currentState actions bestActionSoFar =
        let currentBestValue = lookup currentState bestActionSoFar
        match actions with
        | [] -> bestActionSoFar
        | hd::tl ->
            let currentActionValue = lookup currentState hd
            match currentActionValue with
            | x when x > currentBestValue -> getActionGreedy lookup currentState tl hd
            | _ -> getActionGreedy lookup currentState tl bestActionSoFar
    
    (*
        Either finds the action with the best expected payoff or a random action.
    *)
    let getActionEGreedy (random : System.Random) (lookup : State -> Action -> float) (actions : Action list) epsilon currentState : Option<Action> =
        match actions with
        | [] -> None
        | car::cdr ->
            let randomNumber = random.NextDouble()
            match randomNumber with
            | x when x > epsilon -> Some(getActionGreedy lookup currentState cdr car)
            | _ -> getRandomAction random actions

    let rec playOneRound (gc : GameConfiguration) (Q : Map<(State * Action), float>) epsilon history currentState =
        let isDone = gc.isEndState currentState
        let lookup = gc.lookupFunction Q
        let action =
            let foundAction = getActionEGreedy gc.random lookup (gc.getActions currentState) epsilon currentState
            match foundAction with
            | None -> gc.neutrualAction
            | Some(x) -> x
        
        let reward, newQ =
            match history with
            | [] -> 0.0, (Q.Add ((currentState, action), 0.0))
            | hd::tl ->
                let prevState, prevAction = hd
                let _,_, re = doLearningStep gc.alpha gc.gamma lookup isDone prevState prevAction currentState action
                let nq = Q.Add ((prevState, prevAction), re)
                re, nq

        match isDone with
        | true -> newQ
        | false ->
            let newHistory = (currentState, action) :: history
            let newState, _ = doAction gc.performAction gc.getActions gc.isWinningState gc.rewardFunction gc.getNextAgent currentState action
            playOneRound gc newQ epsilon newHistory newState


    let rec learn (gc : GameConfiguration) (Q : Map<(State * Action), float>) (counter : float) (roundsLeft : int) =
        match roundsLeft with
        | 0 -> Q
        | x when x > 0 ->
            let startState = gc.getStartState()
            let newCounter = counter + 1.0
            let epsilon = gc.calcEpsilon newCounter
            let newQ = playOneRound gc Q epsilon [] startState
            learn gc newQ newCounter (roundsLeft - 1)
        | _ -> failwith "cannot handle negative rounds left"