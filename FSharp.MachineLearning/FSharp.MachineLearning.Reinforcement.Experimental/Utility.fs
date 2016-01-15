namespace Zargess.MachineLearning.ReinforcementLearning

module Utility =
    

    (*
        This function returns the current possition of an agent in the world.
        It needs the current state of the game.
        state               := State

        findAgentPosition   := State -> int
    *)
    let findAgentPosition (state : State) = 
        let id = state.agent.id
        List.findIndex (fun x -> x = id) state.world

