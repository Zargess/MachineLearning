﻿
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

    (*
        TODO : Make this function get either the action with the best reward or a random action.
        If the random value generated is greater than episilon we use the greedy function. If not we use a random value
    *)

    let rec getActionGreedy lookup currentState actions bestActionSoFar =
        let currentBest = lookup (currentState, bestActionSoFar)
        match actions with
        | [] -> currentBest
        | hd::tl ->
            let currentActionValue = lookup (currentState, hd)
            match currentActionValue with
            | x when x > currentBest -> getActionGreedy lookup currentState tl hd
            | _ -> getActionGreedy lookup currentState tl bestActionSoFar

    let getAction (random : System.Random) lookup getPossipleActions epsilon currentState =
        let possipleActions = getPossipleActions currentState
        match possipleActions with
        | [] -> None
        | car::cdr ->
            let randomNumber = random.NextDouble()
            match randomNumber with
            | x when x > epsilon -> Some(getActionGreedy lookup currentState possipleActions car)
            | _ ->
                