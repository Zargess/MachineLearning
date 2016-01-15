namespace Zargess.MachineLearning.ReinforcementLearning

module QLearning =
    val getActionGreedy : (State -> Action -> float) -> State -> Action list -> Action -> Action
    val getRandomAction : System.Random -> Action list -> Action option
    val learn           : GameConfiguration -> Map<(State * Action), float> -> float -> int -> Map<(State * Action), float>
