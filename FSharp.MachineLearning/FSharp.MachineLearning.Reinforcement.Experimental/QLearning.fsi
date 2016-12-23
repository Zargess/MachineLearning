namespace Zargess.MachineLearning.ReinforcementLearning

module QLearning =    
    /// <summary>
    /// Gets the action with the best expected value.
    /// </summary>
    /// <param name="lookup">A function to get the value of a given state action pair.</param>
    /// <param name="currentState">The current game state.</param>
    /// <param name="actions">The available actions for a state.</param>
    /// <returns>The action resulting with best expected value.</returns>
    val getActionGreedy : (State<'a> -> Action -> float) -> State<'a> -> Action list -> Action
    
    /// <summary>
    /// Gets a random action from an list of actions given a way to get a random number.
    /// </summary>
    /// <param name="random">A System.Random object.</param>
    /// <param name="actions">A list of actions.</param>
    /// <returns>An action option. Returns None if actions is empty.</returns>
    val getRandomAction : System.Random -> Action list -> Action option

    /// <summary>
    /// Learns the given model based and the defined game configuration by playing the game the specified number of times.
    /// </summary>
    /// <param name="gc">The configuration descriping the game design.</param>
    /// <param name="rounds">Number of games to play before termination.</param>
    /// <returns>The learned Q map.</returns>
    val learn           : GameConfiguration<'a> -> int -> Map<(State<'a> * Action), float>
