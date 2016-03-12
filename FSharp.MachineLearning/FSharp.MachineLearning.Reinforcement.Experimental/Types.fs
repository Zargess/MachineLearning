namespace Zargess.MachineLearning.ReinforcementLearning

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

type GameConfiguration = {
    isEndState : State -> bool;
    getActions : State -> Action list;
    performAction : State -> Action -> State;
    rewardFunction : State -> float;
    lookupFunction : Map<(State * Action), float> -> State -> Action -> float
    calcEpsilon : float -> float;
    getNextAgent : Agent -> Agent;
    random : System.Random;
    neutrualAction : Action;
    getStartState : unit -> State;
    alpha : float;
    gamma : float
}