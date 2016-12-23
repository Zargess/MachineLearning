namespace Zargess.MachineLearning.ReinforcementLearning

type Agent = {
    id : string
}
    
type Action = {
    value : int
}

type State<'a when 'a : comparison> = {
    world : 'a;
    reward : float;
    agent : Agent
}

type GameConfiguration<'a when 'a : comparison> = {
    isEndState : State<'a> -> bool;
    getActions : State<'a> -> Action list;
    performAction : State<'a> -> Action -> State<'a>;
    rewardFunction : State<'a> -> float;
    lookupFunction : Map<(State<'a> * Action), float> -> State<'a> -> Action -> float
    calcEpsilon : float -> float;
    getNextAgent : Agent -> Agent;
    random : System.Random;
    neutrualAction : Action;
    getStartState : unit -> State<'a>;
    learningRate : float;
    discountFactor : float
}