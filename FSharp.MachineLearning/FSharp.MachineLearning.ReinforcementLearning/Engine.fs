﻿namespace FSharp.MachineLearning.ReinforcementLearning

module Engine =
    type Agent = {
        id : int
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
        This function is to check if the given state is the last state
        func                := State -> bool
        state               := State
    *)
    let isGameDone func state = func state

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
    let doAction performAction getPossipleActions isWinningState rewardFunction getNextAgent state action = 
        let newWorld = performAction state action
        let won = isWinningState newWorld

        let reward = rewardFunction won state
        let nextAgent = getNextAgent state.agent
        let newState = {
            world = newWorld;
            reward = reward;
            agent = nextAgent
        }

        let possipleActions = getPossipleActions state

        (newState, possipleActions)

    (*
        Note: Single agent Q-learning
    *)
    let doLearningStep alpha gamma lookup isDone previousState previousAction currentState currentAction =
        let qsa = lookup previousState previousAction

        let qsap =
            match isDone with
            | true -> 0.0
            | false -> lookup currentState currentAction

        let newQsa = qsa + alpha * ((currentState.reward + gamma) * (qsap - qsa))

        (previousState, previousAction, newQsa)
